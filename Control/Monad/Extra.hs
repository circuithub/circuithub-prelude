{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Control.Monad.Extra
  ( liftMaybe
  , broadcastM
  , broadcastByM
  , broadcastWhenM
  , broadcastNubByM
  , applyFlattenedM
  , whenJust
  , whenNothing
  ) where

import Prelude
import Safe.Extra (fromRightPrefix')
import Control.Monad
import Data.List.Split (chunksOf)
import Data.List.Split.Extra (splitPlaces)
import Data.List.Extra
import Data.Monoid
import Data.Bifunctor (bimap)
import qualified Data.Vector as V

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

assertLength :: String -> [a] -> [b] -> [b]
assertLength !_note _is os =
  if length _is /= length os
  then error (_note <> ": output length " <> (show . length) os <> " does not match input length " <> (show . length) _is)
  else os
{-# INLINE assertLength #-}

-- | Branch similar to 'when', but for 'Just' values
--
-- >>> whenJust (Just "Value") $ \a -> print $ "got Just " ++ show a
-- OUT: got Just "Value"
--
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _  = return ()

-- | Branch similar to 'when', but for 'Nothing' values
--
-- >>> when Nothing $ print "got Nothing"
-- OUT: got Nothing
--
whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing (Just _) _ = return ()
whenNothing Nothing f  = f

-- | Broadcast elements according to the splits (odd lists are sent to the first function, even lists to the second)
--   and gather the results maintaining order
--
--   (TODO: create a parallel version of this function)
--
-- >>> broadcastM [0,2,0,3,4,0,0,1,1,0,2,2]
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
broadcastM :: Monad m => [Int] -> ([a] -> m [b]) -> ([a] -> m [b]) -> [a] -> m [b]
broadcastM seqs f g xs = do
  let seqss     = fromRightPrefix' "broadcastM" $ splitPlaces seqs xs
      (lxs,rxs) = (concat `bimap` concat) (foldr zipChunks ([],[]) $ chunksOf 2 seqss)
  lxs' <- f lxs
  rxs' <- g rxs
  return $ gather seqs (assertLength "broadcastM/f" lxs lxs') (assertLength "broadcastM/g" rxs rxs')
  where
    zipChunks :: [a] -> ([a], [a]) -> ([a], [a])
    zipChunks (x:[]  ) (ls,rs) = (x:ls, rs)
    zipChunks (x:y:[]) (ls,rs) = (x:ls, y:rs)
    zipChunks _       _        = error "broadcastGroupsM/zipChunks"

    gather :: [Int] -> [a] -> [a] -> [a]
    gather []         _  _           = []
    gather (lseqs:[]   ) ls _        = take lseqs ls
    gather (lseqs:rseqs:seqs') ls rs =
      let (ls',ls'') = splitAt lseqs ls
          (rs',rs'') = splitAt rseqs rs
      in ls' ++ rs' ++ gather seqs' ls'' rs''

-- | Broadcast elements by a bucketing function and gather the results maintaining order
--   In line with the standard ordering of Bool (False < True), elements that evaluate to
--   False are sent to the first function and those that evaluate to True to the second.
broadcastByM :: Monad m => (a -> Bool) -> ([a] -> m [b]) -> ([a] -> m [b]) -> [a] -> m [b]
broadcastByM bucket f g xs = broadcastM (length `map` collateBy bucket xs) f g xs

-- | Broadcast elements to a batch function when they meet some condition (return the rest as given)
--
-- >>> broadcastWhenM (> 0) (const Nothing) (\xs -> print xs >> return (map Just xs)) [-1,-4,0,2,3,-4,1,0 :: Int]
-- OUT: [2,3,1]
-- [Nothing,Nothing,Nothing,Just 2,Just 3,Nothing,Just 1,Nothing]
--
broadcastWhenM :: Monad m => (a -> Bool) -> (a -> b) -> ([a] -> m [b]) -> [a] -> m [b]
broadcastWhenM cond def = broadcastByM cond (mapM $ return . def)

-- | Apply a monadic function with nub and then un-nub when done
--
-- >>> broadcastNubByM floor (\xs -> print xs >> return (map (\x -> (floor x, x)) xs)) [0.1, 2.2,-1.2, 0.8, 9.9, 2.5, 3.0, 2.9, 2.1,-1.4]
-- OUT: [0.1,2.2,-1.2,9.9,3.0]
-- [(0,0.1),(2,2.2),(-2,-1.2),(0,0.1),(9,9.9),(2,2.2),(3,3.0),(2,2.2),(2,2.2),(-2,-1.2)]
--
broadcastNubByM :: (Eq i, Ord i, Functor m, Monad m) => (a -> i) -> ([a] -> m [b]) -> [a] -> m [b]
broadcastNubByM index f xs = invNub `fmap` f nubXs
  where
    (nubXs, nubIndex) = ordNubIndexBy index xs
    invNub nubFxs     = let fxs = V.fromList (assertLength "broadcastNubByM" nubXs nubFxs)
                        in  map (\n -> fxs V.! n) nubIndex

-- | Apply a monadic function to a flattened list.
--   This assumes that the monadic function returns the same number of elements as is passed in.
--
--   TODO: change [[a]] to any monomorphic sequence of [a]?
--
-- >>> (\xs -> print xs >> return (map (* (2::Int)) xs)) `applyFlattenedM` ([[],[1],[2,3],[],[4,5,6],[]])
-- OUT: [1 2 3 4 5 6]
-- [[],[2],[4 6],[],[8,10,12],[]]
--
applyFlattenedM :: Monad m => ([a] -> m [b]) -> [[a]] -> m [[b]]
applyFlattenedM f xss = do
  let ls  = map length xss
  xs' <- f $ concat xss
  if length xs' /= sum ls
  then error "applyFlattenedM: f returned a different number of elements than it was given"
  else return $ fromRightPrefix' "applyFlattenedM" $ splitPlaces ls xs'

---- | Apply a monadic function to nub elements and then replicate results to regain the original shape
--broadcastNubM :: (Eq a, Ord a, Functor m, Monad m) => ([a] -> m [b]) -> [a] -> m [b]
--broadcastNubM = broadcastNubByM id

-- | Broadcast elements by lumping elements into several buckets and gather the results maintaining order
--
-- broadcastGroupsM :: (Enum e, Bounded e, Monad m) => [(e, Int)] -> [[a] -> m [b]] -> [a] -> m [b]
--