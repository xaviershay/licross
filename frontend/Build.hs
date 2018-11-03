import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (try)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List
import Data.String (fromString)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Network.HTTP.Types (status200, status404)
import Network.Wai
  ( Application
  , Request(..)
  , pathInfo
  , requestMethod
  , responseFile
  , responseLBS
  )
import Network.Wai.Handler.Warp (run)

type BuildResult = Either ShakeException ()

type BuildHandler = [String] -> IO BuildResult

type Builder = TVar (TQueue String, TChan BuildResult)

main :: IO ()
main = do
  builder <- forkBuilder buildHandler
  -- TODO: Add some logging middleware
  run 8001 (application builder)

buildHandler :: BuildHandler
buildHandler paths = do
  putStrLn $ "Building: " <> Data.List.intercalate ", " paths
  try $
    shake shakeOptions {shakeThreads = 0, shakeVerbosity = Normal} $ do
      want paths
      phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]
      "pkg/elm.js" %> \out -> do
        let src = "src/elm/Main.elm"
        need [src]
        cmd_ "elm make" ("--output=" <> out) src
      "pkg/index.html" %> \out -> do
        let src = "src/html/index.html"
        need [src]
        cmd_ "cp" src out
      "pkg/app.js" %> \out -> do
        files <- getDirectoryFiles "" ["src/js//*.js"]
        need files
        -- TODO: Babel is kind of slow, taking > 500ms for simple concat :(
        -- Want to blame NPM
        cmd_ "babel" files "-o pkg/app.js"

application builder request respond = do
  let rawPath = BS.unpack $ rawPathInfo request
  let pathWithDefault =
        if hasExtension rawPath
          then rawPath
          else addTrailingPathSeparator rawPath </> "index.html"
  let path = "pkg" <> pathWithDefault
  result <- requestBuild builder path
  case result of
    Left (ShakeException {}) ->
      respond $
      responseLBS
        status404
        [(fromString "Content-Type", fromString "text/plain")]
        (fromString "NOPE")
    Right () -> do
      respond $
        -- TODO: Probably some directory traversal VULN
        responseFile
          status200
        -- TODO: Content-Type from file extension
          [(fromString "Content-Type", fromString "text/html")]
          path
          Nothing

-- forkBuilder spawns a looping thread that blocks until one or more requests
-- are queued, then batches the requests, processes them with the provided
-- function, and returns a result to every blocked caller waiting for the
-- request.
forkBuilder :: BuildHandler -> IO Builder
forkBuilder f = do
  builder <- mkBuilder
  forkIO . forever . handleBuildRequest builder $ f
  return builder
  where
    handleBuildRequest :: Builder -> BuildHandler -> IO ()
    handleBuildRequest builder f = do
      newBuilder <- mkBuilder
      (paths, chan) <- atomically (readQueue builder newBuilder)
      result <- f paths
      atomically $ writeTChan chan result
    readQueue :: Builder -> Builder -> STM ([String], TChan BuildResult)
    readQueue builder newBuilder = do
      (pathQueue, chan) <- readTVar newBuilder >>= swapTVar builder
      -- retry until there is at least one element in queue
      peekTQueue pathQueue
      paths <- flushTQueue pathQueue
      return (paths, chan)
    mkBuilder :: IO Builder
    mkBuilder =
      atomically $ do
        q <- newTQueue
        c <- newBroadcastTChan
        newTVar (q, c)

-- requestBuild will ask the builder to build a path, and block until a result
-- is available. The builder may batch requests, but the result is guaranteed
-- to be from a build started after the request was made.
requestBuild :: Builder -> String -> IO BuildResult
requestBuild builder path = do
  receiver <-
    atomically $ do
      (queue, chan) <- readTVar builder
      writeTQueue queue path
      dupTChan chan
  atomically $ readTChan receiver
