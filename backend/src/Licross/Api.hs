{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Licross.Api
  ( runServer
  ) where

import qualified Data.HashMap.Strict as M -- unordered-containers
import Network.Wai -- wai
import Network.Wai.Handler.Warp -- warp
import Network.Wai.Middleware.Cors -- wai-cors
import Network.Wai.Middleware.RequestLogger -- wai-extra
import Servant -- servant-server

import Licross.FakeData
import Licross.Json
import Licross.Prelude
import Licross.Types

type GameAPI = "example" :> Get '[ JSON] RedactedGame

example :: RedactedGame
example = RedactedGame Nothing titleGame

server :: Servant.Server GameAPI
server = return example

gameAPI :: Servant.Proxy GameAPI
gameAPI = Servant.Proxy

app :: Network.Wai.Application
app =
  Network.Wai.Middleware.RequestLogger.logStdoutDev $
  Network.Wai.Middleware.Cors.cors (const . Just $ corsPolicy) $
  Servant.serve gameAPI server
  where
    corsPolicy =
      Network.Wai.Middleware.Cors.simpleCorsResourcePolicy
        { Network.Wai.Middleware.Cors.corsRequestHeaders =
            ["authorization", "content-type"]
        }

runServer :: Int -> IO ()
runServer port = Network.Wai.Handler.Warp.run port app
