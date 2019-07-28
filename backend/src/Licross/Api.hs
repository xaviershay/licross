{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Licross.Api
  ( runServer
  ) where

import qualified Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, readTVar, check, writeTVar) -- stm
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
   = "example" :> RawM
     :<|> "game" :> Post '[ JSON] GameId
     -- TODO: PlayerId will eventually come from authenticated session rather than query param
     :<|> "game" :> Capture "id" GameId :> "join" :> QueryParam "playerId" PlayerId :> Post '[ JSON] ()
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "move" :> ReqBody '[ JSON] Move :> Post '[ JSON] ()
     :<|> "game" :> Capture "id" GameId :> "player" :> Capture "playerId" PlayerId :> "subscribe" :> RawM
     :<|> "game" :> Capture "id" GameId :> "start" :> Post '[ JSON] ()

data State = State
  { games :: TVar (M.HashMap GameId Game)
  , templateGame :: Game
  }

type AppM = ReaderT State Servant.Handler

example :: AppM Application
example = return . eventStreamIO $ \emit -> do
  emit $ Network.Wai.EventSource.ServerEvent
    (Just "snapshot")
    Nothing
    [ Data.Binary.Builder.fromLazyByteString
        (Data.Aeson.encode $ RedactedGame Nothing titleGame)
    ]
  emit $ Network.Wai.EventSource.ServerEvent
    (Just "finished")
    Nothing
    [ mempty ]
  emit $ Network.Wai.EventSource.CloseEvent

joinGame :: GameId -> Maybe PlayerId -> AppM ()
joinGame _ Nothing = throwError $ err400 {errBody = "Must provide playerId" }
joinGame gid (Just pid) = do
  State {games = gs} <- ask
  -- This pattern feels like it could be cleaner. Revisit once more examples of
  -- changing game state.
  updated <- liftIO . atomically $ do
    x <- readTVar gs

    case M.lookup gid x of
      Nothing -> return False
      Just game -> do
        modifyTVar gs (M.adjust (over gameVersion (1 +) . over gamePlayers (M.insert pid (mkPlayer pid "Unknown"))) gid)
        return True

  case updated of
    False -> throwError $ err404 { errBody = "Game not found" }
    True -> return ()

newGame :: AppM GameId
newGame = do
  State {games = gs, templateGame = template} <- ask
  liftIO $ do
    id <- newGameId
    atomically $ modifyTVar gs (M.insert id template)
    return id

postMove :: GameId -> PlayerId -> Move -> AppM ()
postMove = error ""

fillRacks :: Game -> Game
fillRacks game =
  let ps = view gamePlayers game in
  foldl fillRackFor game ps

  where
    fillRackFor game player =
      let rack = view playerRack player in

      foldl (moveTileFromBag player) game [1..(7 - length rack)]

    moveTileFromBag player game n =
      -- TODO: Randomness
      let tile = take 1 . view gameBag $ game in
      let bag = drop 1 . view gameBag $ game in

      -- TODO: handle no tiles

      case tile of
        [x] -> over gameBag (drop 1) . over (gamePlayers . at (view playerId player) . _Just . playerRack) ((:) x) $ game
        _   -> game

startGame :: GameId -> AppM ()
startGame gid = do
  State {games = gs} <- ask
  -- This pattern feels like it could be cleaner. Revisit once more examples of
  -- changing game state.
  updated <- liftIO . atomically $ do
    x <- readTVar gs

    case M.lookup gid x of
      Nothing -> return False
      Just game -> do
        modifyTVar gs (M.adjust (over gameVersion (1 +) . fillRacks) gid)
        return True

  case updated of
    False -> throwError $ err404 { errBody = "Game not found" }
    True -> return ()

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

      let games = atomically $ do
                    x <- readTVar gs
                    return $ M.keys x
      maybeGame <- action

      case maybeGame of
        Nothing -> emit Network.Wai.EventSource.CloseEvent
        Just game -> do
          emit $ Network.Wai.EventSource.ServerEvent
            (Just "snapshot")
            Nothing
            [ Data.Binary.Builder.fromLazyByteString
                (Data.Aeson.encode $ RedactedGame Nothing game)
            ]

          handle gs (lastVersion + 1) emit

server :: Servant.ServerT GameAPI AppM
server = example :<|> newGame :<|> joinGame :<|> postMove :<|> subscribeGame :<|> startGame

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

runServer :: Int -> Game -> IO ()
runServer port templateGame = do
  tvar <- atomically $ newTVar mempty
  let initialState = State tvar templateGame
  Network.Wai.Handler.Warp.run port (app initialState)
