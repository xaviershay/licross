{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Licross.Api
  ( runServer
  ) where

import Debug.Trace

import GHC.Generics
import qualified Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, readTVar, check) -- stm
import Control.Monad.Reader (ReaderT, ask, runReaderT) -- mtl
import Control.Monad.Trans (liftIO) -- mtl
import qualified Data.Aeson
import qualified Data.Binary.Builder
import qualified Data.HashMap.Strict as M -- unordered-containers
import qualified Data.Text;
import Network.HTTP.Types (status200, status400, status404) -- http-types
import Network.Wai -- wai
import qualified Network.Wai.EventSource -- wai-extra
import Network.Wai.Handler.Warp -- warp
import Network.Wai.Middleware.Cors -- wai-cors
import Network.Wai.Middleware.RequestLogger -- wai-extra
import Network.Wai.Handler.WebSockets -- wai-websockets
import Network.WebSockets.Connection -- websockets
import Servant -- servant-server
import Servant.RawM (RawM)

import Licross.FakeData
import Licross.Json
import Licross.Prelude
import Licross.Types

import Data.Aeson
import Data.Attoparsec.Text (parseOnly, endOfInput)
import qualified Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)

newtype GraphQLBody = GraphQLBody AST.Document

instance FromJSON GraphQLBody where
  parseJSON = withObject "GraphQLBody" $ \v -> do
    query <- v .: "query"

    case parseOnly (document <* endOfInput) query of
      Left err -> fail err
      Right ast -> return $ GraphQLBody ast

instance Servant.FromHttpApiData GameId where
  parseUrlPiece x = parseUrlPiece x >>= note "Invalid Game ID" . gameIdFromText

instance Servant.FromHttpApiData PlayerId where
  parseUrlPiece x = parseUrlPiece x >>= return . PlayerId

-- Not using nested APIs here, though it would technically DRY things up. Might
-- revisit, but for now was creating too much conceptual overhead for little
-- benefit.
type GameAPI
   = "example" :> Get '[ JSON] RedactedGame
     :<|> "graphql" :> ReqBody '[JSON] GraphQLBody :> Post '[JSON] GraphQLResponse
     :<|> "graphqlSubscribe" :> RawM
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

data Rate = Rate { __typename :: Data.Text.Text, currency :: Data.Text.Text }
  deriving (Generic)

instance ToJSON Rate

data GraphQLResponse = RatesResponse

instance ToJSON GraphQLResponse where
  toJSON RatesResponse = object
    [ "data" .= object ["rates" .= [Rate "Rate" "USD"]]]


graphqlHandler :: GraphQLBody -> AppM GraphQLResponse
graphqlHandler (GraphQLBody ast) = trace (show ast) $ return RatesResponse

graphqlSubscribe :: AppM Application
graphqlSubscribe  = do
  State {games = gs} <- ask

  return $
    Network.Wai.Handler.WebSockets.websocketsOr
      Network.WebSockets.Connection.defaultConnectionOptions
      (wsApp gs)
      backupApp
  where
    wsApp gs pendingConn = do
      conn <- Network.WebSockets.Connection.acceptRequest pendingConn
      msg <- receive conn
      traceM . show $ msg
      msg <- receive conn
      traceM . show $ msg
      msg <- receive conn
      traceM . show $ msg
      msg <- receive conn
      traceM . show $ msg

      return ()

    --handle gs conn lastVersion = do
    --  let action = atomically $ do
    --                 x <- readTVar gs

    --                 case M.lookup gid x of
    --                   Nothing -> return Nothing
    --                   Just game -> do
    --                     check $ view gameVersion game > lastVersion
    --                     return $ Just game

    --  maybeGame <- action

    --  case maybeGame of
    --    Nothing -> return () -- Terminate connection
    --    Just game -> do
    --       Network.WebSockets.Connection.sendTextData
    --         conn
    --         ((Data.Aeson.encode $ RedactedGame Nothing game) <> "\n")

    --       handle gs conn (view gameVersion game)

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

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

-- SSE version
subscribeGame :: GameId -> PlayerId -> AppM Application
subscribeGame gid pid = do
  State {games = gs} <- ask
  liftIO $ do
    maybeGame <- atomically $ readTVar gs >>= (return . M.lookup gid)
    let game = emptyGame
    return $
      case maybeGame of
        Nothing -> \_ respond -> respond $ responseLBS status404 [] ""
        Just game ->
          Network.Wai.EventSource.eventSourceAppIO $ do
            let x =
                  Network.Wai.EventSource.ServerEvent
                    (Just "snapshot")
                    Nothing
                    [ Data.Binary.Builder.fromLazyByteString
                        (Data.Aeson.encode $ RedactedGame Nothing game)
                    ]
            Control.Concurrent.threadDelay 1000000
            return x

-- Websocket version
subscribeGame2 :: GameId -> PlayerId -> AppM Application
subscribeGame2 gid pid = do
  State {games = gs} <- ask

  return $
    Network.Wai.Handler.WebSockets.websocketsOr
      Network.WebSockets.Connection.defaultConnectionOptions
      (wsApp gs)
      backupApp
  where
    wsApp gs pendingConn = do
      conn <- Network.WebSockets.Connection.acceptRequest pendingConn

      handle gs conn 0

    handle gs conn lastVersion = do
      let action = atomically $ do
                     x <- readTVar gs

                     case M.lookup gid x of
                       Nothing -> return Nothing
                       Just game -> do
                         check $ view gameVersion game > lastVersion
                         return $ Just game

      maybeGame <- action

      case maybeGame of
        Nothing -> return () -- Terminate connection
        Just game -> do
           Network.WebSockets.Connection.sendTextData
             conn
             ((Data.Aeson.encode $ RedactedGame Nothing game) <> "\n")

           handle gs conn (view gameVersion game)

    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

server :: Servant.ServerT GameAPI AppM
server = example :<|> graphqlHandler :<|> graphqlSubscribe :<|> newGame :<|> joinGame :<|> postMove :<|> subscribeGame2

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
  let id = GameId "1"
  putStrLn . show $ id
  tvar <- atomically $ newTVar (M.insert id emptyGame mempty) -- mempty
  let initialState = State tvar
  Network.Wai.Handler.Warp.run port (app initialState)
