{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Licross.Api
  ( runServer
  , applyMove -- TODO: Move
  , testDb
  , testDbInsert
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
import Data.Maybe (fromJust)
import Network.HTTP.Types (status200, status404, hContentType) -- http-types
import Network.Wai -- wai
import qualified Network.Wai.EventSource -- wai-extra
import Network.Wai.Handler.Warp -- warp
import Network.Wai.Middleware.Cors -- wai-cors
import Network.Wai.Middleware.RequestLogger -- wai-extra
import Network.Wai.EventSource.EventStream (eventToBuilder)
import Servant -- servant-server
import Servant.RawM (RawM)
import Servant.RawM.Server

import Licross.FakeData
import Licross.Json
import Licross.Prelude
import Licross.Types






-- TODO: Extract to more appropriate module once we know what we're doing

import Database.Beam hiding (set)
import Database.Beam.Postgres
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Int (Int32(..))
import Database.Beam.Backend.SQL

data GameRecordT f
    = GameRecord
    { _gameRecordId      :: C f GameId
    , _gameRecordRegion  :: C f Text
    , _gameRecordVersion :: C f Int32
    , _gameRecordData    :: C f Game }
    deriving Generic

type GameRecord = GameRecordT Identity
type GameRecordId = PrimaryKey GameRecordT Identity

deriving instance Show GameRecord
deriving instance Eq GameRecord
instance Beamable GameRecordT

instance Table GameRecordT where
   data PrimaryKey GameRecordT f = GameRecordId (C f GameId)
     deriving (Generic, Beamable)
   primaryKey = GameRecordId . _gameRecordId

data LicrossDb f = LicrossDb
                      { _licrossGames :: f (TableEntity GameRecordT) }
                        deriving (Generic, Database be)

licrossDb :: DatabaseSettings be LicrossDb
licrossDb = defaultDbSettings `withDbModification`
              dbModification {
                _licrossGames =
                  modifyTableFields
                    tableModification {
                      _gameRecordId = "id"
                      , _gameRecordRegion = "region"
                      , _gameRecordVersion = "version"
                      , _gameRecordData = "data"
                    }
              }

instance FromBackendRow Postgres GameId where
  fromBackendRow = GameId <$> fromBackendRow

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be GameId where
  sqlValueSyntax (GameId x) = sqlValueSyntax x

instance FromBackendRow Postgres Game where
  fromBackendRow = (\(PgJSONB g) -> g) <$> fromBackendRow

instance HasSqlValueSyntax be (PgJSONB Game) => HasSqlValueSyntax be Game where
  sqlValueSyntax x = sqlValueSyntax (PgJSONB x)

testDbInsert = do
  conn <- connectPostgreSQL "postgres:///licross_development"

  runBeamPostgresDebug putStrLn conn $ do
    runInsert $
      insert (_licrossGames licrossDb) $
        insertValues [ testRecord ]

testRecord = GameRecord @Identity (GameId "abcde") "dev" 2 emptyGame

testDb = do
  conn <- connectPostgreSQL "postgres:///licross_development"
  let allGames = all_ (_licrossGames licrossDb)

  runBeamPostgresDebug putStrLn conn $ do
    games <- runSelectReturningList $ select allGames
    mapM_ (liftIO . putStrLn . show) games
-- END TODO
--
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
     :<|> "game" :> Capture "id" GameId :> "move" :> QueryParam "playerId" PlayerId :> ReqBody '[ JSON] Move :> Post '[ JSON] ()
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

    -- TODO: persist game state to database

  case updated of
    False -> throwError $ err404 { errBody = "Game not found" }
    True -> return ()

-- TODO: Region
toRecord gid g = GameRecord @Identity gid "dev" (view gameVersion g) g

newGame :: AppM GameId
newGame = do
  State {games = gs, templateGame = template} <- ask
  liftIO $ do
    id <- newGameId
    atomically $ modifyTVar gs (M.insert id template)

    conn <- connectPostgreSQL "postgres:///licross_development"

    runBeamPostgresDebug putStrLn conn $ do
      runInsert $
        insert (_licrossGames licrossDb) $
          insertValues [ toRecord id template ]

    return id

postMove :: GameId -> Maybe PlayerId -> Move -> AppM ()
postMove gid pid move = do
  State {games = gs} <- ask

  updated <- liftIO . atomically $ do
    x <- readTVar gs

    case M.lookup gid x of
      Nothing -> return False
      Just game -> do
        modifyTVar gs
          (M.adjust
            ( over gameVersion (1 +)
            . applyMove move
            ) gid
          )
        return True

  -- TODO: persist move to database async (change type of updated to Maybe Game)
  case updated of
    False -> throwError $ err404 { errBody = "Game not found" }
    True -> return ()


applyMove (PlayTiles []) = id
applyMove (PlayTiles (t:ts)) =
  set
    (gameTiles . at (view tileId t) . _Just . tileLocation)
    (view tileLocation t)
  . applyMove (PlayTiles ts)

fillRacks :: Game -> Game
fillRacks game =
  let ps = view gamePlayers game in
  foldl fillRackFor game ps

  where
    inPlayerRack player tile =
      case view tileLocation tile of
        LocationRack pid -> pid == view playerId player
        _ -> False

    fillRackFor game player =
      let rack = filter (inPlayerRack player) (M.elems . view gameTiles $ game) in

      foldl (moveTileFromBag player) game [1..(7 - length rack)]

    inBag :: Tile -> Bool
    inBag tile =
      case view tileLocation tile of
        LocationBag -> True
        _ -> False

    moveTileFromBag player game n =
      -- TODO: Randomness
      let tile = take 1 . filter inBag . M.elems . view gameTiles $ game in

      -- TODO: handle no tiles

      case tile of
        [x] ->
          set
            (gameTiles . at (view tileId x) . _Just . tileLocation)
            (LocationRack $ view playerId player)
          $ game
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
    handle :: TVar (M.HashMap GameId Game) -> Int32 -> (Network.Wai.EventSource.ServerEvent -> IO ()) -> IO ()
    handle gs lastVersion emit = do
      let action = atomically $ do
                      x <- readTVar gs

                      case M.lookup gid x of
                        Nothing -> return Nothing
                        Just game -> do
                          check $ view gameVersion game >= lastVersion
                          return $ Just game

      -- TODO: Try rehydrating game if Nothing
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

          handle gs (view gameVersion game + 1) emit

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
