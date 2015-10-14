{-# LANGUAGE FlexibleContexts #-}
module Data.Either.Validation.Extra
  ( validate
  , intercalateFailure
  , invalidWhen
  , failWithHead
  , successMay
  , failureMay
  , successes
  , failures
  ) where

{- See also
* http://www.reddit.com/r/haskell/comments/34n8gq/smarter_validation/
* http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Applicative-Lift.html
* http://www.reddit.com/r/haskell/comments/3407pb/implementing_a_new_style_of_data_validation/
-}

import Data.Bool (Bool)
import Data.Maybe (Maybe (..), maybe, catMaybes)
import Data.List (map)
import Data.Either (Either (..))
import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import Data.Monoid (Monoid, mempty)
import Control.Monad (Monad, fail, return)
import Data.Either.Validation
-- import Data.Monoid (Monoid)
import Data.Sequences (IsSequence)
import Data.MonoTraversable (Element)
import ClassyPrelude (intercalate, toList)
import Data.Function ((.))

-- | Like note from Control.Error.Util except for 'Validation' instead of 'Either'
--   I.e. tag the 'Nothing' value of a 'Maybe'.
validate :: e -> Maybe a -> Validation [e] a
validate msg = maybe (Failure [msg]) Success

-- TODO: Tag the Nothing value of a MaybeT
-- validateT :: Monad m => a -> MaybeT m b -> ValidationT a m b

-- | Reformat a validation failure to produce an Either
--   The errors are concatenated by intercalating a separator string
intercalateFailure :: (IsSequence (Element e), IsSequence e)
                    => Element e
                    -> Validation e a
                    -> Either (Element e) a
intercalateFailure sep = first (intercalate sep . toList) . validationToEither

-- | Mark a field invalid
invalidWhen :: Bool -> e -> Validation [e] ()
invalidWhen cond msg =
  if cond
  then Failure [msg]
  else Success ()

-- | Fail in a monad with the first validation failure
failWithHead :: Monad m => Validation [Text] a -> m a
failWithHead (Success x    ) = return x
failWithHead (Failure (e:_)) = fail (unpack e)
failWithHead (Failure _    ) = fail "Unknown failure (failWithHead)"

-- | Convert a @Success a@ into a @Just a@
successMay :: Validation e a -> Maybe a
successMay (Success x) = Just x
successMay (Failure _) = Nothing

-- | Convert a @Failure e@ into a @Just e@
failureMay :: Validation e a -> Maybe e
failureMay (Success _) = Nothing
failureMay (Failure x) = Just x

-- | Convert a list of 'Validation' into a list of successes
successes :: [Validation e a] -> [a]
successes = catMaybes . map successMay

-- | Convert a list of 'Validation' into a list of failures
failures :: [Validation e a] -> [e]
failures = catMaybes . map failureMay
