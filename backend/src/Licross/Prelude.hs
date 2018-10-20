module Licross.Prelude
  ( module Control.Lens
  , module Data.Function
  , module Data.Ord
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
