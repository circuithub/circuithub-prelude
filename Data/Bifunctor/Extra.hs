module Data.Bifunctor.Extra where

import Data.Bifunctor (Bifunctor, bimap)

-- | A more generic version of 'mapBoth' from Data.Either.Combinators and 'both' from Data.Tuple.Extra
--   This is equivalent to 'join bimap'.
mapBoth :: Bifunctor p => (a -> b) -> p a a -> p b b
mapBoth f = f `bimap` f
