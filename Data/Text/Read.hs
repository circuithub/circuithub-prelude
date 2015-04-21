{-# LANGUAGE TypeFamilies #-}
module Data.Text.Read
  ( readValid
  , readEither
  ) where

import Data.Either.Validation (Validation, eitherToValidation)
import Data.MonoTraversable (MonoFoldable, Element)
import ClassyPrelude (readMay)
import Control.Error.Util (note)
import Text.Read (Read)
import Data.Function ((.))
import Data.Char (Char)

-- | A read that either fails or succeeds in 'Validation'
readValid :: (Element txt ~ Char, MonoFoldable txt, Read a)
          => e
          -> txt
          -> Validation e a
readValid e = eitherToValidation . note e . readMay

-- | A read that either fails or succeeds in 'Either'
readEither  :: (Element txt ~ Char, MonoFoldable txt, Read a)
            => e
            -> txt
            -> Either e a
readEither e = note e . readMay
