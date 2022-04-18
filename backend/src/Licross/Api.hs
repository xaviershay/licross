{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Licross.Api
  ( runServer
  ) where

import qualified Control.Concurrent
import Control.Concurrent.STM
  ( TVar
  , atomically
  , check
  , modifyTVar
  , newTVar
  , readTVar
  , writeTVar
  ) -- stm
import Control.Monad (forever, join)
import Control.Monad.Reader (ReaderT, ask, runReaderT) -- mtl
import Control.Monad.Trans (liftIO) -- mtl
import qualified Data.Aeson
import qualified Data.Binary.Builder
import qualified Data.HashMap.Strict as M -- unordered-containers
import Debug.Trace
import Network.HTTP.Types (status200, status404) -- http-types
import Network.Wai -- wai
import qualified Network.Wai.EventSource -- wai-extra
import Network.Wai.Handler.Warp -- warp
import Network.Wai.Middleware.Cors -- wai-cors
import Network.Wai.Middleware.RequestLogger -- wai-extra
import Servant -- servant-server
import Servant.RawM (RawM')
import qualified Servant.RawM.Server

import Licross.FakeData
import Licross.Json
import Licross.Prelude
import Licross.Types

instance Servant.FromHttpApiData GameId where
  parseUrlPiece x = parseUrlPiece x >>= note "Invalid Game ID" . gameIdFromText

instance Servant.FromHttpApiData PlayerId where
  parseUrlPiece x = parseUrlPiece x >>= return . PlayerId

-- Not using nested APIs here, though it would technically DRY things up. Might
-- revisit, but for now was creating too much conceptual overhead for little
-- benefit.
type GameAPI
   = "example" :> Subscription RedactedGame
     :<|> "game" :> Post '[ JSON] GameId
     :<|> "game" :> Capture "id" GameId :> "join" :> Post '[ JSON] PlayerId
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "move" :> ReqBody '[ JSON] Move :> Post '[ JSON] ()
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "subscribe" :> Subscription RedactedGame

newtype State = State
  { games :: TVar (M.HashMap GameId Game)
  }

type AppM = ReaderT State Servant.Handler

type Subscription a = RawM' a

subscribeExample :: AppM Network.Wai.Application
subscribeExample = do
  return $ Network.Wai.EventSource.eventStreamAppRaw handle
  where
    handle emit flush = do
      emit $
        Network.Wai.EventSource.ServerEvent
          (Just "snapshot")
          Nothing
          [ Data.Binary.Builder.fromLazyByteString
              (Data.Aeson.encode $ RedactedGame Nothing titleGame)
          ]
      flush
      forever $ Control.Concurrent.threadDelay 100000000

joinGame :: GameId -> AppM PlayerId
joinGame = error ""

newGame :: AppM GameId
newGame = do
  State {games = gs} <- ask
  liftIO $ do
    id <- newGameId
    atomically $ modifyTVar gs (M.insert id emptyGame)
    return id

postMove :: GameId -> PlayerId -> Move -> AppM ()
postMove = error ""

subscribeGame :: GameId -> PlayerId -> AppM Network.Wai.Application
subscribeGame gid pid = do
  currentTVar <- liftIO $ atomically $ newTVar 0
  State {games = gs} <- ask
  return $ Network.Wai.EventSource.eventStreamAppRaw (handle gs currentTVar)
  where
    handle gs currentTVar emit flush = do
      join $
        atomically $ do
          maybeGame <- readTVar gs >>= (return . M.lookup gid)
          case maybeGame of
            Nothing ->
              return $ do
                emit $
                  Network.Wai.EventSource.ServerEvent
                    (Just "not-found")
                    Nothing
                    [ Data.Binary.Builder.fromLazyByteString
                        (Data.Aeson.encode ())
                    ]
                emit Network.Wai.EventSource.CloseEvent
                flush
            Just gamex -> do
              currentT <- readTVar currentTVar
              check (view gameT gamex > currentT)
              writeTVar currentTVar (view gameT gamex)
              return $ do
                emit $
                  Network.Wai.EventSource.ServerEvent
                    (Just "snapshot")
                    Nothing
                    [ Data.Binary.Builder.fromLazyByteString
                        (Data.Aeson.encode $ RedactedGame Nothing gamex)
                    ]
                flush
                handle gs currentTVar emit flush

server :: Servant.ServerT GameAPI AppM
server =
  subscribeExample :<|> newGame :<|> joinGame :<|> postMove :<|> subscribeGame

gameAPI :: Servant.Proxy GameAPI
gameAPI = Servant.Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Network.Wai.Application
app s =
  Network.Wai.Middleware.RequestLogger.logStdoutDev $
  Network.Wai.Middleware.Cors.cors (const . Just $ corsPolicy) $
  Servant.serve gameAPI (hoistServer gameAPI (nt s) server)
  where
    corsPolicy =
      Network.Wai.Middleware.Cors.simpleCorsResourcePolicy
        { Network.Wai.Middleware.Cors.corsRequestHeaders =
            ["authorization", "content-type"]
        }

runServer :: Int -> IO ()
runServer port = do
  tvar <- atomically $ newTVar mempty
  let initialState = State tvar
  Network.Wai.Handler.Warp.run port (app initialState)
