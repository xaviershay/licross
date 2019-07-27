{-# LANGUAGE OverloadedStrings #-}

module Main where

-- aeson
import qualified Data.Aeson

-- base
import qualified System.Environment

-- unordered-containers
import qualified Data.HashMap.Strict as M

import qualified Data.ByteString.Lazy

-- licross
import           Licross.Api
import           Licross.Prelude
import           Licross.Types

main :: IO ()
main = do
  args <- System.Environment.getArgs

  gameTemplate <-
    case args of
      [] -> return $ Left "No filename"
      (f:_) ->
        do
          putStrLn $ "Loading template: " <> f
          contents <- Data.ByteString.Lazy.readFile f
          return $ Data.Aeson.eitherDecode contents

  game <-
    case gameTemplate of
      Left x ->
        do
          putStrLn x
          return emptyGame
      Right g -> return g

  Licross.Api.runServer 8080 game
