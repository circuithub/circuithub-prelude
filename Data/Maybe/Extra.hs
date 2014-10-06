module Data.Maybe.Extra where

import ClassyPrelude
import Data.Maybe
import Data.Bifunctor

sameMaybe :: Maybe a -> Maybe b -> Bool
x `sameMaybe` y = isNothing x == isNothing y

filterFirstJusts :: [(Maybe a, b)] -> [(a,b)]
filterFirstJusts = map (fromJust `bimap` id) . (filter $ isJust . fst)

filterSecondJusts :: [(a, Maybe b)] -> [(a,b)]
filterSecondJusts = map (id `bimap` fromJust) . (filter $ isJust . snd)

filterFirstNothings :: [(Maybe a, b)] -> [b]
filterFirstNothings = map snd . (filter $ isNothing . fst)

filterSecondNothings :: [(a, Maybe b)] -> [a]
filterSecondNothings = map fst . (filter $ isNothing . snd)
