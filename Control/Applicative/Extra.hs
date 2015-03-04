{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Control.Applicative.Extra
  ( dispatchA
  , dispatchByA
  , dispatchWhenA
  ) where

import Prelude (error)
import Data.List
import Data.String
import Data.Bool
import Data.Eq
import Data.Function
import Data.Int
-- import Data.Ord
import Data.Functor
import Safe.Extra (fromRightPrefix')
-- import Control.Monad
import Control.Applicative
import Data.Traversable (traverse)
import Data.List.Split (chunksOf)
import Data.List.Split.Extra (splitPlaces)
import Data.List.Extra (collateBy)
-- import Data.Monoid
import Data.Bifunctor (bimap)
import Text.Show (show)

assertLengthEq :: String -> [a] -> [b] -> [b]
assertLengthEq !_note _is os =
  if length _is /= length os
  then error (_note ++ ": output length " ++ (show . length) os ++ " does not match input length " ++ (show . length) _is)
  else os
{-# INLINE assertLengthEq #-}

-- | Dispatch elements according to the splits (odd lists are sent to the first function, even lists to the second)
--   and gather the results maintaining order
--
--   (TODO: create a parallel version of this function)
--
-- >>> dispatchA [0,2,0,3,4,0,0,1,1,0,2,2]
--                (\xs -> print xs >> return (map (+ 0.1) xs))
--                (\xs -> print xs >> return (map (+ 0.9) xs))
--                [ 19,29 , 39,49,59 , 11,21,31,41 , 69 , 51 , 61,71 , 79,89 ]
-- OUT: [11.0,21.0,31.0,41.0,51.0,61.0,71.0]
-- OUT: [19.0,29.0,39.0,49.0,59.0,69.0,79.0,89.0]
-- [19.9,29.9,39.9,49.9,59.9,11.1,21.1,31.1,41.1,69.9,51.1,61.1,71.1,79.9,89.9]
--
-- >>> splitPlaces [0,2,0,3,4,0,0,1,1,0,2,2] [ 19,29 , 39,49,59 , 11,21,31,41 , 69 , 51 , 61,71 , 79,89 ]
-- [ [],[19,29] , [],[39,49,59] , [11,21,31,41],[] , [],[69] , [51],[] , [61,71],[79,89] ]
--
-- >>> foldr zipChunks ([],[]) $ chunksOf 2 $ [[],[19,29],[],[39,49,59],[11,21,31,41],[],[],[69],[51],[],[61,71],[79,89]]
-- ( [[],[],[11,21,31,41],[],[51],[61,71]] , [[19,29],[39,49,59],[],[69],[],[79,89]] )
--
dispatchA :: Applicative m => [Int] -> ([a] -> m [b]) -> ([a] -> m [b]) -> [a] -> m [b]
dispatchA seqs f g xs =
  let seqss     = fromRightPrefix' "dispatchA" $ splitPlaces seqs xs
      (lxs,rxs) = (concat `bimap` concat) (foldr zipChunks ([],[]) $ chunksOf 2 seqss)
      lxs' = f lxs
      rxs' = g rxs
  in gather seqs <$> (assertLengthEq "dispatchA/f" lxs <$> lxs') <*> (assertLengthEq "dispatchA/g" rxs <$> rxs')
  where
    zipChunks :: [a] -> ([a], [a]) -> ([a], [a])
    zipChunks (x:[]  ) (ls,rs) = (x:ls, rs)
    zipChunks (x:y:[]) (ls,rs) = (x:ls, y:rs)
    zipChunks _       _        = error "dispatchA/zipChunks"

    gather :: [Int] -> [a] -> [a] -> [a]
    gather []         _  _           = []
    gather (lseqs:[]   ) ls _        = take lseqs ls
    gather (lseqs:rseqs:seqs') ls rs =
      let (ls',ls'') = splitAt lseqs ls
          (rs',rs'') = splitAt rseqs rs
      in ls' ++ rs' ++ gather seqs' ls'' rs''

-- | Dispatch elements by a bucketing function and gather the results maintaining order
--   In line with the standard ordering of Bool (False < True), elements that evaluate to
--   False are sent to the first function and those that evaluate to True to the second.
dispatchByA :: Applicative m => (a -> Bool) -> ([a] -> m [b]) -> ([a] -> m [b]) -> [a] -> m [b]
dispatchByA bucket f g xs = dispatchA (length `map` collateBy bucket xs) f g xs

-- | Dispatch elements to a batch function when they meet some condition (return the rest as given)
--
-- >>> dispatchWhenA (> 0) (const Nothing) (\xs -> print xs >> return (map Just xs)) [-1,-4,0,2,3,-4,1,0 :: Int]
-- OUT: [2,3,1]
-- [Nothing,Nothing,Nothing,Just 2,Just 3,Nothing,Just 1,Nothing]
--
dispatchWhenA :: Applicative m => (a -> Bool) -> (a -> b) -> ([a] -> m [b]) -> [a] -> m [b]
dispatchWhenA cond def = dispatchByA cond (traverse $ pure . def)

-- | Dispatch elements by lumping elements into several buckets and gather the results maintaining order
--
-- dispatchGroupsA :: (Enum e, Bounded e, Applicative m) => [(e, Int)] -> [[a] -> m [b]] -> [a] -> m [b]
--

