{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Licross.Api
  ( runServer
  ) where

import qualified Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, readTVar, check) -- stm
import Control.Concurrent.Chan
import Control.Monad.Reader (ReaderT, ask, runReaderT) -- mtl
import Control.Monad.Trans (liftIO) -- mtl
import qualified Data.Aeson
import qualified Data.Binary.Builder
import qualified Data.HashMap.Strict as M -- unordered-containers
import Data.IORef
import Network.HTTP.Types (status200, status404, hContentType) -- http-types
import Network.Wai -- wai
import qualified Network.Wai.EventSource -- wai-extra
import Network.Wai.Handler.Warp -- warp
import Network.Wai.Middleware.Cors -- wai-cors
import Network.Wai.Middleware.RequestLogger -- wai-extra
import Network.Wai.EventSource.EventStream (eventToBuilder)
import Servant -- servant-server
import Servant.RawM (RawM)

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
   = "example" :> Get '[ JSON] RedactedGame
     :<|> "game" :> Post '[ JSON] GameId
     :<|> "game" :> Capture "id" GameId :> "join" :> Post '[ JSON] PlayerId
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "move" :> ReqBody '[ JSON] Move :> Post '[ JSON] ()
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "subscribe" :> RawM

newtype State = State
  { games :: TVar (M.HashMap GameId Game)
  }

type AppM = ReaderT State Servant.Handler

example :: AppM RedactedGame
example = return $ RedactedGame Nothing titleGame

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

-- TODO: Move to extras dir, contribute back
eventStreamIO :: ((Network.Wai.EventSource.ServerEvent -> IO()) -> IO ()) -> Application
eventStreamIO handler _ respond = respond $ responseStream status200 [(hContentType, "text/event-stream")] (streamer handler)
  where
    streamer handler sendChunk flush = do
      handler emit

      where
        emit :: Network.Wai.EventSource.ServerEvent -> IO ()
        emit event = do
           case eventToBuilder event of
             Nothing -> return ()
             Just b  -> sendChunk b >> flush

myApp :: Application
myApp = do
  eventStreamIO (handle 0)

  where
    handle :: Int -> (Network.Wai.EventSource.ServerEvent -> IO ()) -> IO ()
    handle counter emit = do
      emit $ Network.Wai.EventSource.ServerEvent
               (Just "snapshot")
               Nothing
               [ Data.Binary.Builder.fromLazyByteString
                   (Data.Aeson.encode $ counter)
               ]

      Control.Concurrent.threadDelay 1000000

      handle 1 emit

subscribeGame :: GameId -> PlayerId -> AppM Application
subscribeGame gid pid = do
  State {games = gs} <- ask

  return $ eventStreamIO (handle gs 0)

  where
    handle :: TVar (M.HashMap GameId Game) -> Int -> (Network.Wai.EventSource.ServerEvent -> IO ()) -> IO ()
    handle gs lastVersion emit = do
      let action = atomically $ do
                      x <- readTVar gs

                      case M.lookup gid x of
                        Nothing -> return Nothing
                        Just game -> do
                          check $ view gameVersion game >= lastVersion
                          return $ Just game

      maybeGame <- action

      emit $ case maybeGame of
               Nothing -> Network.Wai.EventSource.CloseEvent
               Just game ->
                 Network.Wai.EventSource.ServerEvent
                   (Just "snapshot")
                   Nothing
                   [ Data.Binary.Builder.fromLazyByteString
                       (Data.Aeson.encode $ RedactedGame Nothing game)
                   ]

      handle gs (lastVersion + 1) emit

server :: Servant.ServerT GameAPI AppM
server = example :<|> newGame :<|> joinGame :<|> postMove :<|> subscribeGame

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
