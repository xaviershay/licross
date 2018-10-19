module Main where

import Licross.FakeData
import Licross.Types

import qualified Data.Text as T

main :: IO ()
main = putStrLn . T.unpack . showBoard . gameBoard $ emptyGame

focus = main
