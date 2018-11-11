module Licross.Prelude
  ( module Control.Lens
  , module Data.Function
  , module Data.Ord
  , module Data.List
  , note
  , shush
  ) where

-- lens
--
-- We use lens a lot, but try to avoid the operators and instead stick to named
-- functions.
import Control.Lens
  ( view
  , over
  , set
  , at
  , ix
  , _Just
  )

-- commond base functions
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

shush :: Either e a -> Maybe a
shush = either (const Nothing) Just
