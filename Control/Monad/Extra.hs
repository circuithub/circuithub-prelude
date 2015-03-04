{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Control.Monad.Extra
  ( liftMaybe
  , whenJust
  , whenNothing
  ) where

import Data.Maybe
import Control.Monad

-- | TODO: Document
liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

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
