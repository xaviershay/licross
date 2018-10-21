{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Licross.Api
  ( runServer
  ) where

-- text
import qualified Data.Text as T

-- servant-server
import Servant

-- wai
import Network.Wai
import Network.Wai.Handler.Warp

-- licross
import Licross.Prelude
import Licross.Types
import Licross.Json

type GameAPI = "example" :> Get '[JSON] RedactedGame

example :: RedactedGame
example = RedactedGame Nothing emptyGame

server :: Servant.Server GameAPI
server = return example

gameAPI :: Servant.Proxy GameAPI
gameAPI = Servant.Proxy

app :: Network.Wai.Application
app = Servant.serve gameAPI server

runServer :: Int -> IO ()
runServer port = Network.Wai.Handler.Warp.run port app
