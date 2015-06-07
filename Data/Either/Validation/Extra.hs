{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Either.Validation.Extra
  ( validate
  , intercalateFailure
  , invalidWhen
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
import Data.Char (Char)
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
-- import Control.Monad.Error.Class (MonadError) -- TODO
import GHC.Exts (IsList, Item, toList)

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

-- | Fail in a monad with the first validation failure
-- TODO: failWithHead :: (IsString e, MonadError e m) => Validation [e] a -> m a
--       failWithHead (Failure (e:_)) = mfail (toString e)
--       failWithHead (Failure _    ) = mfail ("Unknown failure (failWithHead)")
failWithHead :: (Monad m, IsList e, Item e ~ Char) => Validation [e] a -> m a
failWithHead (Success x    ) = return x
failWithHead (Failure (e:_)) = fail (toList e)
failWithHead (Failure _    ) = fail "Unknown failure (failWithHead)"
