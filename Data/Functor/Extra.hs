{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Data.Functor.Extra
  ( applyFlattened
  , broadcastNubBy
  , broadcastNub
  ) where

import Prelude (error)
import Data.Functor
import Data.List
import Data.String
import Data.Int
import Data.Eq
import Data.Ord
import Data.Function
import Safe.Extra (fromRightPrefix')
import Data.List.Extra (ordNubIndexBy)
import Data.List.Split.Extra (splitPlaces)
import qualified Data.Vector as V
import Text.Show (show)

assertLength :: String -> Int -> [b] -> [b]
assertLength !_note l os =
  if l /= length os
  then error (_note ++ ": output length " ++ (show . length) os ++ " does not match input length " ++ show l)
  else os
{-# INLINE assertLength #-}

-- | Apply a functorial (or, in many typical use cases, a monadic) function to a flattened list.
--   This assumes that the monadic function returns the same number of elements as is passed in.
--
--   TODO: Generalize?
--    * change [[a]] to any monomorphic sequence of [a]?
--    * change [[a]] to Foldable g => g [a]?
--
-- >>> (\xs -> print xs >> return (map (* (2::Int)) xs)) `applyFlattened` ([[],[1],[2,3],[],[4,5,6],[]])
-- OUT: [1 2 3 4 5 6]
-- [[],[2],[4 6],[],[8,10,12],[]]
--
applyFlattened :: Functor f => ([a] -> f [b]) -> [[a]] -> f [[b]]
applyFlattened f xss =
  let ls  = length <$> xss
      xs' = f $ concat xss
  in (fromRightPrefix' "applyFlattened" . splitPlaces ls . assertLength "applyFlattened" (sum ls))
      <$> xs'

-- | Apply a functorial (or, in many typical use cases, a monadic) function with nub and then un-nub when done
--
-- >>> broadcastNubBy floor (\xs -> print xs >> return (map (\x -> (floor x, x)) xs)) [0.1, 2.2,-1.2, 0.8, 9.9, 2.5, 3.0, 2.9, 2.1,-1.4]
-- OUT: [0.1,2.2,-1.2,9.9,3.0]
-- [(0,0.1),(2,2.2),(-2,-1.2),(0,0.1),(9,9.9),(2,2.2),(3,3.0),(2,2.2),(2,2.2),(-2,-1.2)]
--
broadcastNubBy :: (Eq i, Ord i, Functor f) => (a -> i) -> ([a] -> f [b]) -> [a] -> f [b]
broadcastNubBy index f xs = invNub `fmap` f nubXs
  where
    (nubXs, nubIndex) = ordNubIndexBy index xs
    invNub nubFxs     = let fxs = V.fromList (assertLength "broadcastNubByA" (length nubXs) nubFxs)
                        in  map (\n -> fxs V.! n) nubIndex

-- | Apply a monadic function to nub elements and then replicate results to regain the original shape
broadcastNub :: (Eq a, Ord a, Functor f) => ([a] -> f [b]) -> [a] -> f [b]
broadcastNub = broadcastNubBy id
