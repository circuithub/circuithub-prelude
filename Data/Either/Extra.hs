module Data.Either.Extra where

import Prelude
import Data.Either

sameEither :: Either a b -> Either c d -> Bool
x `sameEither` y = isLeft x == isLeft y

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x
