{-# LANGUAGE OverloadedStrings #-}

module Main where

-- unordered-containers
import qualified Data.HashMap.Strict as M

import Licross.Prelude
import Licross.Types
import Licross.Api

main :: IO ()
main = do
  let gameTemplate = emptyGame
  Licross.Api.runServer 8080 gameTemplate
