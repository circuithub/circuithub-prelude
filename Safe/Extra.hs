module Safe.Extra 
  ( fromLeftNote'
  , fromRightNote'
  , zipWithExact3Note
  , zipWithExact4Note
  , zipWithExact5Note
  ) where

import Prelude
import Data.Either.Combinators
import Data.List (zipWith4, zipWith5)

liftNote :: (a -> b) -> (a -> Bool) -> String -> String -> (a -> b)
liftNote func test caller note val =
  if test val
  then error $ "Pattern match failure, " ++ caller ++ ", " ++ note
  else func val

fromLeftNote' :: String -> Either a b -> a
fromLeftNote' = liftNote fromLeft' isRight "fromLeft Right"

fromRightNote' :: String -> Either a b -> b
fromRightNote' = liftNote fromRight' isLeft "fromRight Left"

zipWithExact3Note :: String -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWithExact3Note note f as bs cs =
  let ls = [length as, length bs, length cs]
  in  if minimum ls /= maximum ls
      then error $ "zipWith3 lists are different lengths " ++ show ls ++ ", " ++ note
      else zipWith3 f as bs cs

zipWithExact4Note :: String -> (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWithExact4Note note f as bs cs ds =
  let ls = [length as, length bs, length cs, length ds]
  in  if minimum ls /= maximum ls
      then error $ "zipWith4 lists are different lengths " ++ show ls ++ ", " ++ note
      else zipWith4 f as bs cs ds

zipWithExact5Note :: String -> (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWithExact5Note note f as bs cs ds es =
  let ls = [length as, length bs, length cs, length ds, length es]
  in  if minimum ls /= maximum ls
      then error $ "zipWith5 lists are different lengths " ++ show ls ++ ", " ++ note
      else zipWith5 f as bs cs ds es
