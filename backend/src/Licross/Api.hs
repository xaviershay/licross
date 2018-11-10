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

instance Servant.FromHttpApiData GameId where
  parseUrlPiece x = parseUrlPiece x >>= return . GameId

instance Servant.FromHttpApiData PlayerId where
  parseUrlPiece x = parseUrlPiece x >>= return . PlayerId

-- Not using nested APIs here, though it would technically DRY things up. Might
-- revisit, but for now was creating too much conceptual overhead for little
-- benefit.
type GameAPI
   = "example" :> Get '[ JSON] RedactedGame
     :<|> "game" :> Post '[ JSON] GameId
     :<|> "game" :> Capture "id" GameId :> "join" :> Post '[ JSON] PlayerId
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "move" :> ReqBody '[ JSON] Move :> Post '[ JSON] ()
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "subscribe" :> Raw

example :: Servant.Handler RedactedGame
example = return $ RedactedGame Nothing titleGame

joinGame :: GameId -> Handler PlayerId
joinGame = error ""

newGame :: Handler GameId
newGame = GameId <$> pure 4

postMove :: GameId -> PlayerId -> Move -> Handler ()
postMove = error ""

subscribeGame :: GameId -> PlayerId -> Server Raw
subscribeGame = error ""

server :: Servant.Server GameAPI
server = example :<|> newGame :<|> joinGame :<|> postMove :<|> subscribeGame

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
