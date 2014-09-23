{-# LANGUAGE BangPatterns #-}
module Data.List.Extra 
  ( nubOrd
  , nubOrdBy
  , nubKey
  , nubHash
  , nubIndexOrd
  , nubIndexOrdBy
  , nubIndexHash
  , collateCycleBy
  , collateBy
  , collate
  , lookupWithHash
  , lookupWithHashNote
  , lookupWithHashDebug
  , lookupFromNothing
  , lookupFromNothings
  , sortAndGroup
  , sortedGroupBy
  , minIndexMay
  ) where

import Prelude
import Safe
import Data.Bifunctor
import qualified Data.Set            as S
import qualified Data.HashSet        as HS
import qualified Data.IntMap.Strict  as IM
import qualified Data.Map.Strict     as M
import qualified Data.HashMap.Strict as HM
import Data.ComparableKey (OrdByKey (..), OrdKeyed (..))
import Data.Hashable
import Data.List
import qualified Data.List.NonEmpty as LN

-- | A faster O(n log n) nub function that works on Ord elements (from https://github.com/nh2/haskell-ordnub#dont-use-nub)
--   See also https://groups.google.com/forum/#!topic/haskell-cafe/4UJBbwVEacg
nubOrd :: Ord a => [a] -> [a]
nubOrd l = go S.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `S.member` s then go s xs
                                    else x : go (S.insert x s) xs

-- | Non-overloaded version of nubOrd
--   TODO: See also the classy-prelude version that has an explicit eq argument
nubOrdBy :: Ord o => (a -> o) -> [a] -> [a]
nubOrdBy f l = go S.empty l
  where
    go _ []     = []
    go s (x:xs) = let fx = f x
                  in if fx `S.member` s then go s xs
                                        else x : go (S.insert fx s) xs

-- | Removes duplicate elements from a list using key equality. Only the first occurrence of each element is retained.
nubKey :: OrdByKey a => [a] -> [a]
nubKey = map (\(OrdKeyed x) -> x) . nubOrd . map OrdKeyed

-- | Another O(n log n) implementation of nub using a hashable instance
nubHash :: (Hashable a, Eq a) => [a] -> [a]
nubHash l = go HS.empty l
  where
    go _ []     = []
    go s (x:xs) = if x `HS.member` s then go s xs
                                     else x : go (HS.insert x s) xs

-- | A version of nub which returns an additional index into the original configuration
--   TODO: In theory we could generalize Integral to Enum here, but unfortunately this would also require 
--         Bounded which is not always desirable
--
-- >>> nubIndexOrdBy (`div` 2) [0,1,4,3,1,2,0,8,1,8,4]
-- ([0,4,3,8],[0,0,1,2,0,2,0,3,0,3,1])
--
nubIndexOrdBy :: (Eq o, Ord o, Integral i) => (a -> o) -> [a] -> ([a], [i])
nubIndexOrdBy f = go M.empty 0
  where
    go _ _ []     = ([],[])
    go s i (x:xs) = let fx = f x
                    in case fx `M.lookup` s of
                      Just i' -> found    s      i i' xs
                      Nothing -> notfound s fx x i    xs
    found    s      i i' xs = let (xs',is) = go s i xs
                              in  (xs',i':is)
    notfound s fx x i    xs = let (xs',is) = go (M.insert fx i s) (i + 1) xs
                              in  (x:xs',i:is)

--
-- >>> nubIndexOrd [0,1,4,3,1,2,0,8,1,8,4]
-- ([0,1,4,3,2,8],[0,1,2,3,1,4,0,5,1,5,2])
--
nubIndexOrd :: (Eq a, Ord a, Integral i) => [a] -> ([a], [i])
nubIndexOrd = nubIndexOrdBy id

--
-- >>> nubIndexHashBy (`div` 2) [0,1,4,3,1,2,0,8,1,8,4]
-- ([0,4,3,8],[0,0,1,2,0,2,0,3,0,3,1])
--
nubIndexHashBy :: (Eq h, Hashable h, Integral i) => (a -> h) -> [a] -> ([a], [i])
nubIndexHashBy f = go HM.empty 0
  where
    go _ _ []     = ([],[])
    go s i (x:xs) = let fx = f x
                    in case fx `HM.lookup` s of
                      Just i' -> found    s      i i' xs
                      Nothing -> notfound s fx x i    xs
    found    s      i i' xs = let (xs',is) = go s i xs
                              in  (xs',i':is)
    notfound s fx x i    xs = let (xs',is) = go (HM.insert fx i s) (i + 1) xs
                              in  (x:xs',i:is)

--
-- >>> nubIndexHash [0,1,4,3,1,2,0,8,1,8,4]
-- ([0,1,4,3,2,8],[0,1,2,3,1,4,0,5,1,5,2])
--
nubIndexHash :: (Eq a, Hashable a, Integral i) => [a] -> ([a], [i])
nubIndexHash = nubIndexHashBy id

-- | Split a list into successive groups of elements mapped onto a cycle of collate indexes
--   For example, you might use this function to split a list into successive (false/true) pairs
--
-- >>> collateCycleBy [False .. True] (> 0) [1,3,3,-4,8,-6,-4,-9,8,8]
-- [[],[1,3,3],[-4],[8],[-6,-4,-9],[8,8]]
--
-- >>> collateCycleBy [1 .. 5] id [1,3,3,4,2,4,4,1]
-- [[1],[],[3,3],[4],[],[],[2],[],[4,4],[],[1],[],[],[],[]]
--
collateCycleBy :: Eq i => [i] -> (a -> i) -> [a] -> [[a]]
collateCycleBy _  _ [] = []
collateCycleBy is f xs =
  let (!matchess, !remainder) = spanGroup f is xs in matchess ++ collateCycleBy is f remainder
  where
    spanGroup :: Eq i => (a -> i) -> [i] -> [a] -> ([[a]], [a])
    spanGroup _ []     xs' = ([],xs')
    spanGroup g (j:js) xs' =
      let (!matches,  remainder ) = span ((== j) . g) xs'
          (!matchess, remainder') = spanGroup g js remainder
      in  (matches : matchess, remainder')

-- | A more restricted version of collateCycleBy which uses the index type to determine a range
--
-- >>> collateBy (> 0) [1,3,-4,8,-6,-4,-9,3,3]
-- [[],[1,3],[-4],[8],[-6,-4,-9],[3,3]]
--
-- >>> collateBy id ([1,3,3,4,2,4,4,0,1] :: Word8)
-- [[],[1],[],[3,3],[4],...,[],[],[2],...,[],[],[],[],[4,4],...,[0],[1],...]
--
collateBy :: (Enum i, Bounded i, Eq i) => (a -> i) -> [a] -> [[a]]
collateBy f = collateCycleBy [minBound..maxBound] f

-- | A more restricted version of collateBy that uses the list elements directly as the collate index
--
-- >>> collate [True,False,False,True,False,True,True,True,False]
-- [[],[True],[False,False],[True],[False],[True,True,True],[False],[]]
--
-- >>> collate ([1,3,3,4,2,4,4,0,1] :: Word8)
-- [[],[1],[],[3,3],[4],...,[],[],[2],...,[],[],[],[],[4,4],...,[0],[1],...]
--
collate :: (Enum a, Bounded a, Eq a) => [a] -> [[a]]
collate = collateBy id

-- | Create a lookup function from a key hash function and a key-value list.
lookupWithHash :: (a -> Int) -> [(a,b)] -> a -> Maybe b
lookupWithHash f kvs y = IM.lookup (f y) xm
  where xm = IM.fromList $ map (first f) kvs

-- | Unsafe version of 'lookupWithHash' that assumes the element must exist.
lookupWithHashNote :: String -> (a -> Int) -> [(a,b)] -> a -> b
lookupWithHashNote note f kvs = fromJustNote note . lookupWithHash f kvs

-- | Use only for debugging
lookupWithHashDebug :: Show a => String -> (a -> Int) -> [(a,b)] -> a -> b
lookupWithHashDebug note f kvs k = (fromJustNote (note ++ ' ' : show k ++ ' ' : show (f k)) . lookupWithHash f kvs) k

-- | Lookup right-hand side of a pair when it is 'Nothing' (using the left-hand side as the key to the supplied lookup function).
lookupFromNothing :: (a -> b) -> (a, Maybe b) -> (a, b)
lookupFromNothing f (x, Nothing) = (x, f x)
lookupFromNothing _ (x, Just y)  = (x, y)

-- | Look up values to fill the holes in the right-hand sides of a list of pairs (using the left-hand side as the key to the supplied lookup function).
lookupFromNothings :: (a -> b) -> [(a, Maybe b)] -> [(a, b)]
lookupFromNothings = map . lookupFromNothing

-- | Go from association list [(a,b)] to Map a [b] by sorting & grouping on fst
sortAndGroup :: Ord a => [(a, b)] -> M.Map a [b]
sortAndGroup al = M.fromListWith (++) [(k, [v]) | (k, v) <- al]

-- | Group by that works globally across the list, ie groupBy . sort
-- returns grouped elements as Non-empty lists
sortedGroupBy :: (a -> a-> Ordering) -> [a] -> [LN.NonEmpty a]
sortedGroupBy cmp =
  LN.groupBy equals . sortBy cmp
  where
    equals = ((==EQ) . ) . cmp

-- | Find the index of the minimum element
minIndexMay :: Ord a => [a] -> Maybe Int
minIndexMay []  = Nothing
minIndexMay [_] = return 0
minIndexMay ns  = return . fst $ loop 0 ns
  where
    loop _ []                     = error "impossible state reached in minIndexMay loop"
    loop i [x]                    = (i    , x)
    loop i [x1,x2]    | x1 < x2   = (i    , x1)
                      | otherwise = (i + 1, x2)
    loop i (x1:x2:xs) = let ix'@(_  , x' ) = if x1 < x2 then (i, x1) else (i + 1, x2)
                            ix''@(_ , x'') = loop (i + 2) xs
                        in if x' < x'' then ix' else ix''
