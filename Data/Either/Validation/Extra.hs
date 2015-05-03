{-# LANGUAGE FlexibleContexts #-}
module Data.Either.Validation.Extra
  ( validate
  , intercalateFailure
  , invalidWhen
  , validIfEmpty
  , failWithHead
  ) where

{- See also
* http://www.reddit.com/r/haskell/comments/34n8gq/smarter_validation/
* http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Applicative-Lift.html
* http://www.reddit.com/r/haskell/comments/3407pb/implementing_a_new_style_of_data_validation/
-}

import Data.Bool (Bool)
import Data.Eq (Eq, (==))
import Data.Maybe (Maybe, maybe)
import Data.Either (Either (..))
import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import Data.Monoid (Monoid, mempty)
import Control.Monad (Monad, fail, return)
import Data.Either.Validation
-- import Data.Monoid (Monoid)
import Data.Sequences (IsSequence)
import Data.MonoTraversable (Element)
import ClassyPrelude (intercalate)
import Data.Function ((.))

-- | Like note from Control.Error.Util except for 'Validation' instead of 'Either'
--   I.e. tag the 'Nothing' value of a 'Maybe'.
validate :: e -> Maybe a -> Validation [e] a
validate msg = maybe (Failure [msg]) Success

-- TODO: Tag the Nothing value of a MaybeT
-- validateT :: Monad m => a -> MaybeT m b -> ValidationT a m b

-- | Reformat a validation failure to produce an Either
--   The errors are concatenated by intercalating a separator string
intercalateFailure :: (Monoid (Element e), IsSequence e)
                    => Element e
                    -> Validation e a
                    -> Either (Element e) a
intercalateFailure sep = first (intercalate sep) . validationToEither

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
