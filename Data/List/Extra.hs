{-# LANGUAGE BangPatterns #-}
module Data.List.Extra
  ( keyNub
  , ordNubOn
  , ordNubIndex
  , ordNubIndexBy
  , nubIndexHash
  , collateCycleBy
  , collateBy
  , collate
  , hashLookupWith
  , hashLookupWithNote
  , hashLookupWithDebug
  , lookupFromNothing
  , lookupFromNothings
  , groupOn
  , ordGroupAllOn
  , ordGroupAllByOn
  , zipWithOn
  , zipOn
  , zipMaybe
  , ordZipWithAllOn
  , ordZipAllOn
  , minIndexMay
  , maxIndexMay
  , filterFirstJusts
  , filterSecondJusts
  , filterBothJusts
  , filterFirstNothings
  , filterSecondNothings
  ) where

import           ClassyPrelude       (ordNub, ordNubBy)
import           Data.Bifunctor
import           Data.Ord            (comparing)
import           Prelude
import           Safe
-- import qualified Data.Set            as S
-- import qualified Data.HashSet        as HS
import           Data.ComparableKey  (OrdByKey (..), OrdKeyed (..))
import           Data.Function       (on)
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict  as IM
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe
-- import qualified Data.List.NonEmpty as LN

-- | A faster O(n log n) nub function that works on Ord elements (from https://github.com/nh2/haskell-ordnub#dont-use-nub)
--   See also https://groups.google.com/forum/#!topic/haskell-cafe/4UJBbwVEacg
--   TODO: remove in favour of ordNub in classy-prelude?
-- nubOrd :: Ord a => [a] -> [a]
-- nubOrd l = go S.empty l
--   where
--     go _ []     = []
--     go s (x:xs) = if x `S.member` s then go s xs
--                                     else x : go (S.insert x s) xs

-- | Non-overloaded version of nubOrd
--   TODO: See also the classy-prelude version that has an explicit eq argument
--   TODO: remove in favour of ordNubBy in classy-prelude?
-- nubOrdBy :: Ord o => (a -> o) -> [a] -> [a]
-- nubOrdBy f l = go S.empty l
--   where
--     go _ []     = []
--     go s (x:xs) = let fx = f x
--                   in if fx `S.member` s then go s xs
--                                         else x : go (S.insert fx s) xs

-- | Removes duplicate elements from a list using key equality. Only the first occurrence of each element is retained.
keyNub :: OrdByKey a => [a] -> [a]
keyNub = map (\(OrdKeyed x) -> x) . ordNub . map OrdKeyed

-- | Simpler version of nubOrdBy that uses the default overloaded == operation
--   TODO: Optimize (f is called too many times)
ordNubOn :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
ordNubOn f = ordNubBy f (\x y -> f x == f y)

-- | Another O(n log n) implementation of nub using a hashable instance
--   TODO: remove in favour of hashNub in classy-prelude?
-- nubHash l = go HS.empty l
--   where
--     go _ []     = []
--     go s (x:xs) = if x `HS.member` s then go s xs
--                                      else x : go (HS.insert x s) xs

-- | A version of nub which returns an additional index into the original configuration
--   TODO: In theory we could generalize Integral to Enum here, but unfortunately this would also require
--         Bounded which is not always desirable
--
-- >>> ordNubIndexBy (`div` 2) [0,1,4,3,1,2,0,8,1,8,4]
-- ([0,4,3,8],[0,0,1,2,0,2,0,3,0,3,1])
--
ordNubIndexBy :: (Eq o, Ord o, Integral i) => (a -> o) -> [a] -> ([a], [i])
ordNubIndexBy f = go M.empty 0
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
-- >>> ordNubIndex [0,1,4,3,1,2,0,8,1,8,4]
-- ([0,1,4,3,2,8],[0,1,2,3,1,4,0,5,1,5,2])
--
ordNubIndex :: (Eq a, Ord a, Integral i) => [a] -> ([a], [i])
ordNubIndex = ordNubIndexBy id

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
-- >>> map length $ collateBy (> 0) [1,3,-4,8,-6,-4,-9,3,3]
-- [0,2,1,1,3,2]
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
hashLookupWith :: (a -> Int) -> [(a,b)] -> a -> Maybe b
hashLookupWith f kvs y = IM.lookup (f y) xm
  where xm = IM.fromList $ map (first f) kvs

-- | Unsafe version of 'hashLookupWith' that assumes the element must exist.
hashLookupWithNote :: String -> (a -> Int) -> [(a,b)] -> a -> b
hashLookupWithNote note f kvs = fromJustNote note . hashLookupWith f kvs

-- | Use only for debugging
hashLookupWithDebug :: Show a => String -> (a -> Int) -> [(a,b)] -> a -> b
hashLookupWithDebug note f kvs k = (fromJustNote (note ++ ' ' : show k ++ ' ' : show (f k)) . hashLookupWith f kvs) k

-- | Lookup right-hand side of a pair when it is 'Nothing' (using the left-hand side as the key to the supplied lookup function).
lookupFromNothing :: (a -> b) -> (a, Maybe b) -> (a, b)
lookupFromNothing f (x, Nothing) = (x, f x)
lookupFromNothing _ (x, Just y)  = (x, y)

-- | Look up values to fill the holes in the right-hand sides of a list of pairs (using the left-hand side as the key to the supplied lookup function).
lookupFromNothings :: (a -> b) -> [(a, Maybe b)] -> [(a, b)]
lookupFromNothings = map . lookupFromNothing

-- | Group elements using a conversion instead a comparison function
--
-- >>> groupOn id [1,1,2,2,3,4,5,4,3,2,1,11,22,11,22,11,22,11] :: [[Int]]
-- [[1,1],[2,2],[3],[4],[5],[4],[3],[2],[1],[11],[22],[11],[22],[11],[22],[11]]
--

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

-- | An O(n log n) implementation of 'groupAllOn' that returns an ordinal for each element in order
-- See https://github.com/snoyberg/mono-traversable/issues/36
--
-- >>> ordGroupAllOn id [1,2,3,4,5,4,3,2,1,11,22,11,22,11,22,11] :: [[Int]]
-- [[1,1],[2,2],[3,3],[4,4],[5],[11,11,11,11],[22,22,22]]
--

-- ordGroupAllOn :: Eq b => (Element seq -> b) -> seq -> [seq]
-- ordGroupAllOn f = fmap fromList . groupAllOn f . otoList

ordGroupAllOn :: Ord b => (a -> b) -> [a] -> [[a]]
ordGroupAllOn f = let cmp = comparing snd
                  in (map . map) fst . groupBy (((==EQ) .) . cmp) . sortBy cmp . map (\x -> (x, f x))

-- | Similar to 'ordGroupAllOn', but uses a distance function to do the grouping
--   See https://github.com/snoyberg/mono-traversable/issues/36
--
-- >>> ordGroupAllByOn (\x y -> abs (x - y) < 1.1) (* 0.5) [1,2,3,4,5,4,3,2,1,11,22,11,22,11,22,11] :: [[Double]]
-- [[1.0,1.0,2.0,2.0,3.0,3.0],[4.0,4.0,5.0],[11.0,11.0,11.0,11.0],[22.0,22.0,22.0]]
--
ordGroupAllByOn :: Ord b => (b -> b -> Bool) -> (a -> b) -> [a] -> [[a]]
ordGroupAllByOn dist f = let cmp = comparing snd
                             eq  = dist `on` snd
                         in (map . map) fst . groupBy eq . sortBy cmp . map (\x -> (x, f x))

-- | Zip two list using the supplied pairing function to ensure that the left-hand side matches the right-hand side
--   TODO: See also http://hackage.haskell.org/package/these for an alternative idea
--
-- >>> zipWithOn (\x y -> x * 10 == y) (+) [1,2,3,4,6] ([10,20,40,50,60] :: [Int])
-- [Right 11, Right 22, Left (Left 3), Right 44, Left (Left 6), Left (Right 50), Left (Right 60)]
--
zipWithOn :: (a -> b -> Bool) -> (a -> b -> r) -> [a] -> [b] -> [Either (Either a b) r]
zipWithOn _    _ xs     []          = map (Left . Left) xs
zipWithOn _    _ []     ys          = map (Left . Right) ys
zipWithOn comp f (x:xs') ys@(y:ys') = if x `comp` y
                                      then Right (f x y) : zipWithOn comp f xs' ys'
                                      else (Left . Left) x : zipWithOn comp f xs' ys

-- | Zip two list using the supplied pairing function to ensure that the left-hand side matches the right-hand side
--   TODO: See also http://hackage.haskell.org/package/these for an alternative idea
--
-- >>> zipOn (\x y -> x * 10 == y) [1,2,3,4,6] ([10,20,40,50,60] :: [Int])
-- [Right (1,10), Right (2,20), Left (Left 3), Right (4,40), Left (Left 6), Left (Right 50), Left (Right 60)]
--
zipOn :: (a -> b -> Bool) -> [a] -> [b] -> [Either (Either a b) (a,b)]
zipOn comp = zipWithOn comp (,)

-- | Zip two list using the supplied pairing function. The lists need not be aligned, it will be automatically sorted.
--   TODO: See if it's possible to remove @Ord a@ and @Ord b@ constraints in future.
--
-- >>> ordZipWithAllOn (\x y -> (x * 10) `compare` y) (+) [1,2,3,4,6] ([10,20,40,50,60] :: [Int])
-- [Right 11, Right 22, Left (Left 3), Right 44, Left (Right 50), Right 66]
--
ordZipWithAllOn :: (Ord a, Ord b) => (a -> b -> Ordering) -> (a -> b -> r) -> [a] -> [b] -> [Either (Either a b) r]
ordZipWithAllOn comp f as bs = ordZipAllOn' (sort as) (sort bs)
  where
    ordZipAllOn' xs     []             = map (Left . Left) xs
    ordZipAllOn' []     ys             = map (Left . Right) ys
    ordZipAllOn' xs@(x:xs') ys@(y:ys') =
      case x `comp` y of
        EQ -> Right (x `f` y) : ordZipAllOn' xs' ys'
        LT -> Left (Left x) : ordZipAllOn' xs' ys
        GT -> Left (Right y) : ordZipAllOn' xs ys'

-- | Zip two list using the supplied pairing function. The lists need not be aligned, it will be automatically sorted.
--   TODO: See if it's possible to remove @Ord a@ and @Ord b@ constraints in future.
--
-- >>> ordZipAllOn (\x y -> (x * 10) `compare` y) [1,2,3,4,6] ([10,20,40,50,60] :: [Int])
-- [Right (1,10), Right (2,20), Left (Left 3), Right (4,40), Left (Right 50), Right (6, 60)]
--
ordZipAllOn :: (Ord a, Ord b) => (a -> b -> Ordering) -> [a] -> [b] -> [Either (Either a b) (a,b)]
ordZipAllOn comp = ordZipWithAllOn comp (,)

-- | Zip 2 lists together, ensuring the length of the resulting list is equal
--   to the longest of the two by padding with Nothing
-- TODO: see also http://hackage.haskell.org/package/these-0.3/docs/Data-Align.html and https://www.reddit.com/r/haskell/comments/37qwr0/what_are_the_inconsistencies_and_inconveniences/crpqqwt
zipMaybe :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMaybe [] _bs = map (\x -> (Nothing, Just x)) _bs
zipMaybe _as [] = map (\x -> (Just x, Nothing)) _as
zipMaybe (a:as) (b:bs) = (Just a, Just b) : zipMaybe as bs

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

-- | Find the index of the maximum element
maxIndexMay :: Ord a => [a] -> Maybe Int
maxIndexMay []  = Nothing
maxIndexMay [_] = return 0
maxIndexMay ns  = return . fst $ loop 0 ns
  where
    loop _ []                     = error "impossible state reached in maxIndexMay loop"
    loop i [x]                    = (i    , x)
    loop i [x1,x2]    | x1 > x2   = (i    , x1)
                      | otherwise = (i + 1, x2)
    loop i (x1:x2:xs) = let ix'@(_  , x' ) = if x1 > x2 then (i, x1) else (i + 1, x2)
                            ix''@(_ , x'') = loop (i + 2) xs
                        in if x' > x'' then ix' else ix''

-- * Filters on lists of pairs

filterFirstJusts :: [(Maybe a, b)] -> [(a,b)]
filterFirstJusts = map (fromJust `bimap` id) . (filter $ isJust . fst)

filterSecondJusts :: [(a, Maybe b)] -> [(a,b)]
filterSecondJusts = map (id `bimap` fromJust) . (filter $ isJust . snd)

filterBothJusts :: [(Maybe a, Maybe b)] -> [(a,b)]
filterBothJusts = map (fromJust `bimap` fromJust) . (filter $ \(x,y) -> isJust x && isJust y)

filterFirstNothings :: [(Maybe a, b)] -> [b]
filterFirstNothings = map snd . (filter $ isNothing . fst)

filterSecondNothings :: [(a, Maybe b)] -> [a]
filterSecondNothings = map fst . (filter $ isNothing . snd)
