{-# LANGUAGE TupleSections #-}
module Data.Tuple.Extra where

import Data.Functor

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

sequenceFst :: (Functor f) => (f a, b) -> f (a, b)
sequenceFst (a,b) = fmap (,b) a
sequenceSnd :: (Functor f) => (a, f b) -> f (a, b)
sequenceSnd (a,b) = fmap (a,) b