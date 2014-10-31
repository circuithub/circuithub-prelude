module Data.Maybe.Extra
  ( sameMaybe
  , nothingIf
  , justIf
  ) where

import Prelude
import Data.Maybe (isNothing)

sameMaybe :: Maybe a -> Maybe b -> Bool
x `sameMaybe` y = isNothing x == isNothing y

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f x = if f x then Nothing else Just x

justIf :: (a -> Bool) -> a -> Maybe a
justIf f x = if f x then Just x else Nothing
