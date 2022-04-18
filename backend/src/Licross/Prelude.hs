module Licross.Prelude
  ( module Control.Lens
  , module Data.Function
  , module Data.Ord
  , module Data.List
  , trace
  , traceM
  , note
  , shush
  , randomId
  ) where

-- lens
--
-- We use lens a lot, but try to avoid the operators and instead stick to named
-- functions.
import Control.Lens (_Just, at, ix, over, set, view)

-- commond base functions
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Debug.Trace

-- internal base functions
import qualified Data.Char
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Random

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

shush :: Either e a -> Maybe a
shush = either (const Nothing) Just

randomId :: Int -> IO T.Text
randomId n = do
  let alphabet = base32alphabet
  chars <-
    V.replicateM n $ do
      System.Random.randomRIO (0, V.length alphabet - 1) >>=
        return . (V.!) alphabet
  return . T.pack . V.toList $ chars

base32alphabet = V.fromList . map Data.Char.chr $ [65 .. 90] ++ [50 .. 55]
