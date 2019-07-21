import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (try)
import Control.Monad (forM_, forever)
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
import System.FSNotify
import qualified System.Posix.Signals -- unix

type BuildResult = Either ShakeException ()

type BuildHandler = [String] -> IO BuildResult

type Builder = TVar (TQueue String, TChan BuildResult)

-- Abstract these so that they can be used to generate both file watching rules
-- and build rules.
data BuildRule = BuildRule
  { sourceDir :: FilePath
  , target :: FilePath
  , rule :: BuildRule -> Development.Shake.Action ()
  }

buildRules =
  [ BuildRule "src/js" "pkg/app.js" $ \rule -> do
      files <- getDirectoryFiles "" [sourceDir rule </> "*.js"]
      need files
      -- TODO: Babel is kind of slow, taking > 500ms for simple concat :(
      -- Want to blame NPM
      cmd_ "babel" files "-o" (target rule)
  , BuildRule "src/elm" "pkg/elm.js" $ \rule -> do
      let src = sourceDir rule </> "Main.elm"
      files <- getDirectoryFiles "" [sourceDir rule </> "*.elm"]
      need files
      -- TODO: Allow for debug/release builds, optimize code for latter.
      cmd_ "elm make" ("--output=" <> target rule) src
  , BuildRule "src/html" "pkg/index.html" $ \rule -> do
      let src = sourceDir rule </> "index.html"
      need [src]
      cmd_ "cp" src (target rule)
  ]

main :: IO ()
main = do
  builder <- forkBuilder buildHandler
  -- TODO: Add some logging middleware
  -- As an optimization, trigger builds on file change. A subsequent web
  -- request will trigger a rebuild, but there should be no changes so it will
  -- be fast.
  watcherId <-
    forkIO . System.FSNotify.withManagerConf defaultConfig $ \mgr -> do
      forM_ buildRules $ \buildRule -> do
        watchTree
          mgr
          (sourceDir buildRule)
          -- Some editors (e.g. vim) don't write files directly so generate weird
          -- events. This check appears to sufficiently limit us to one event per
          -- change, though that event type may be weird (e.g. `Removed` on file
          -- save).
          (hasExtension . eventPath)
          -- We don't actually need to wait for the result here, but it's fine if
          -- we do - just keeps an extra thread around.
          (const $ requestBuild builder (target buildRule) >> pure ())
      -- Block indefinitely
      forever $ threadDelay 1000000
  -- Ensure that threads are cleaned up when developing in GHCI. Kill thread is
  -- dangerous, but it's (TODO) only for dev mode so don't care. In production,
  -- threads will be killed by default when the process is killed.
  tid <- myThreadId
  System.Posix.Signals.installHandler
    System.Posix.Signals.keyboardSignal
    (System.Posix.Signals.Catch $ killThread watcherId >> killThread tid)
    Nothing
  putStrLn "Starting dev server on :8001"
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
      forM_ buildRules $ \buildRules -> do
        (target buildRules) %> (const $ rule buildRules buildRules)

application builder request respond = do
  let rawPath = BS.unpack $ rawPathInfo request
  let pathWithDefault =
        if hasExtension rawPath
          then rawPath
          else "/index.html"
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
