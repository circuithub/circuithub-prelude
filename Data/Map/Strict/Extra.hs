module Data.Map.Strict.Extra where

import Prelude
import Data.Map.Strict as M

-- | Construct a map to lists (similar to a multimap)
fromListMulti :: Ord k => [(k, a)] -> Map k [a]
fromListMulti [] = M.empty
fromListMulti ((xk,xa):xs) = M.map reverse $ insertAll (M.singleton xk [xa]) xs
  where
    f y (Just ys) = Just $ y : ys
    f y Nothing   = Just $ [y]
    insertAll m []           = m
    insertAll m ((yk,ya):ys) = insertAll (M.alter (f ya) yk m) ys
