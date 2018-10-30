import Control.Exception (try)
import Control.Concurrent.MVar (newMVar, withMVar)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Network.HTTP.Types (status200, status404)
import Network.Wai
  ( Application
  , Request(..)
  , pathInfo
  , requestMethod
  , responseLBS
  , responseFile
  )
import Network.Wai.Handler.Warp (run)

import Data.String (fromString)

-- Use a mutex here because Shake is single threaded. For bonus points, would
-- be neat to queue up requested paths and batch them all into the next run.
-- What would also be neat is to trigger a rebuild on save but block until
-- complete on request.
application mutex request respond = withMVar mutex $ \_ -> do
  let rawPath = BS.unpack $ rawPathInfo request

  let pathWithDefault = if hasExtension rawPath then
                          rawPath
                        else
                          addTrailingPathSeparator rawPath </> "index.html"

  let path = "pkg" <> pathWithDefault

  result <-
    try $
    shake shakeOptions {shakeThreads = 0, shakeVerbosity = Normal} $ do
      want $ [path]
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

main :: IO ()
main = do
  mutex <- newMVar ()

  -- TODO: Add some logging middleware
  run 8001 (application mutex)
