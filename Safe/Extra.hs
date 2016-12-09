module Safe.Extra
  ( fromLeftNote'
  , fromRightNote'
  , fromRightPrefix'
  , zipWithExact3Note
  , zipWithExact4Note
  , zipWithExact5Note
  , zipExact3Note
  , zipExact4Note
  , zipExact5Note
  ) where

import Prelude
import Data.Either.Combinators
import Data.List (zipWith4, zipWith5)
import GHC.Stack (HasCallStack)

liftNote
  :: HasCallStack
  => (a -> b)
  -> (a -> Bool)
  -> String
  -> (String -> a -> String)
  -> String
  -> (a -> b)
liftNote func test caller fnNote note val =
  if test val
  then error $ "Pattern match failure, " ++ caller ++ ", " ++ fnNote note val
  else func val

fromLeftNote' :: HasCallStack => String -> Either a b -> a
fromLeftNote' = liftNote fromLeft' isRight "fromLeft Right" const

fromRightNote' :: HasCallStack => String -> Either a b -> b
fromRightNote' = liftNote fromRight' isLeft "fromRight Left" const

-- | Prefix the Left error with a note
fromRightPrefix' :: (HasCallStack, Show e) => String -> Either e b -> b
fromRightPrefix' = liftNote fromRight' isLeft "fromRight" $ \msg (Left e) -> msg ++ ": Left " ++ show e

zipWithExact3Note :: HasCallStack => String -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWithExact3Note note f as bs cs =
  let ls = [length as, length bs, length cs]
  in  if minimum ls /= maximum ls
      then error $ "zipWith3 lists are different lengths " ++ show ls ++ ", " ++ note
      else zipWith3 f as bs cs

zipWithExact4Note :: HasCallStack => String -> (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWithExact4Note note f as bs cs ds =
  let ls = [length as, length bs, length cs, length ds]
  in  if minimum ls /= maximum ls
      then error $ "zipWith4 lists are different lengths " ++ show ls ++ ", " ++ note
      else zipWith4 f as bs cs ds

zipWithExact5Note :: HasCallStack => String -> (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWithExact5Note note f as bs cs ds es =
  let ls = [length as, length bs, length cs, length ds, length es]
  in  if minimum ls /= maximum ls
      then error $ "zipWith5 lists are different lengths " ++ show ls ++ ", " ++ note
      else zipWith5 f as bs cs ds es

zipExact3Note :: HasCallStack => String -> [a] -> [b] -> [c] -> [(a,b,c)]
zipExact3Note note = zipWithExact3Note note (,,)

zipExact4Note :: HasCallStack => String -> [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zipExact4Note note = zipWithExact4Note note (,,,)

zipExact5Note :: HasCallStack => String -> [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zipExact5Note note = zipWithExact5Note note (,,,,)
