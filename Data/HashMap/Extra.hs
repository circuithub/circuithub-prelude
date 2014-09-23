module Data.HashMap.Extra where

import Prelude 
import Data.HashMap.Strict as H
import Data.Hashable
import Data.Bifunctor

mapKeys :: (Hashable b, Eq b) => (a -> b) -> HashMap a c -> HashMap b c 
mapKeys f = H.fromList . Prelude.map (first f) . H.toList  

-- todo find idiomatic way to do this

--maybeMapKeys :: (Hashable b, Eq b) => (a -> Maybe b) -> HashMap a c -> Maybe (HashMap b c)
--maybeMapKeys f = H.fromList . mapM (first f) . H.toList  
--  where 
--    ff f t = case f t 
--      (Just x, y)  -> Just (x,y)
--      _            -> Nothing 
--instance Bifunctor H.HashMap where
--  first f = H.fromList . Prelude.map (first f) . H.toList  
--  second f = H.map f