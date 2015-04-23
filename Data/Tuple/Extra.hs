{-# LANGUAGE TupleSections #-}
module Data.Tuple.Extra where

import Data.Functor

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w,x,y,z) = f w x y z

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (v, w,x,y,z) = f v w x y z

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (u,v,w,x,y,z) = f u v w x y z

sequenceFst :: (Functor f) => (f a, b) -> f (a, b)
sequenceFst (a,b) = fmap (,b) a
sequenceSnd :: (Functor f) => (a, f b) -> f (a, b)
sequenceSnd (a,b) = fmap (a,) b