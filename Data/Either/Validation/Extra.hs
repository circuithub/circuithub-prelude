module Data.Either.Validation.Extra
  ( validate
  , invalidWhen
  , validIfEmpty
  , failWithHead
  ) where

{- TODO: Ask on stackoverflow why Validation is not a monad?
-}

import Data.Bool (Bool)
import Data.Eq (Eq, (==))
import Data.Maybe (Maybe, maybe)
import Data.Text (Text, unpack)
import Data.Monoid (Monoid, mempty)
import Control.Monad (Monad, fail, return)
import Data.Either.Validation

-- | Like note from Control.Error.Util except for 'Validation' instead of 'Either'
--   I.e. tag the 'Nothing' value of a 'Maybe'.
validate :: e -> Maybe a -> Validation [e] a
validate msg = maybe (Failure [msg]) Success

-- TODO: Tag the Nothing value of a MaybeT
-- validateT :: Monad m => a -> MaybeT m b -> ValidationT a m b

-- | Mark a field invalid
invalidWhen :: Bool -> e -> Validation [e] a
invalidWhen cond msg =
  if cond
  then Failure [msg]
  else mempty

-- | Turn a validation with empty failures into a success
validIfEmpty :: (Eq e, Monoid e) => a -> Validation e a -> Validation e a
validIfEmpty x v@(Failure e) = if e == mempty then Success x else v
validIfEmpty _ v             = v

-- | Fail in a monad with the first validation failure
failWithHead :: Monad m => Validation [Text] a -> m a
failWithHead (Success x    ) = return x
failWithHead (Failure (e:_)) = fail (unpack e)
failWithHead (Failure _    ) = fail "Unknown failure (failWithHead)"
