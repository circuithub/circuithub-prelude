{-# LANGUAGE TypeFamilies #-}
module Data.Text.Read
  ( readNote
  , readNoteWith
  , readNoteExpect
  , readValidate
  , readValidateWith
  , readValidateExpect
  ) where

import ClassyPrelude (readMay, asTypeOf)
import Data.Either (Either)
import Data.Either.Validation (Validation, eitherToValidation)
import Data.MonoTraversable (MonoFoldable, Element, otoList)
import Data.Sequences (IsSequence)
import Control.Error.Util (note)
import Text.Read (Read)
import Data.Function (($), (.))
import Data.Char (Char)
import Data.Bifunctor (first)
import Data.Monoid (Monoid, (<>))
import GHC.Exts (IsString, fromList, fromString)

-- | A read that either fails or succeeds in 'Either'
-- >>> readNote "Expected Int" "x"
-- Left "Expected Int"
--
readNote  :: (Element txt ~ Char, MonoFoldable txt, Read a)
          => e
          -> txt
          -> Either e a
readNote e = note e . readMay

--
-- >>> readNoteWith ("Expected Int, received: " ++) "x"
-- Left "Expected Int, received: x"
--
readNoteWith  :: (Element txt ~ Char, MonoFoldable txt, Read a)
              => (txt -> e)
              -> txt
              -> Either e a
readNoteWith fe t = readNote (fe t) t

--
-- >>> readNoteExpect "Int" "x"
-- Left "Expected Int, received: \"x\""
--
readNoteExpect  :: (IsString e, Monoid e, Element txt ~ Char, MonoFoldable txt, Read a) -- IsString e,
                => e
                -> txt
                -> Either e a
readNoteExpect expect s = readNote
                            ( fromString "Expected " `asTypeOf` expect
                              <> expect
                              <> fromString ", received: " `asTypeOf` expect
                              -- <> fromString s `asTypeOf` expect
                            )
                            s

-- | A read that either fails or succeeds in 'Validation'
-- >>> readValidate (\s -> "Expected Int, received: " ++ s) `traverse` splitOn "," "1,2,3,df,6,3,gbud,$$,8,9,19"
-- Failure ["Expected Int, received: df", "Expected Int, received: gbud", "Expected Int, received: $$"]
--
readValidate  :: (Element txt ~ Char, MonoFoldable txt, Read a)
              => e
              -> txt
              -> Validation [e] a
readValidate e = first (:[]) . eitherToValidation . readNote e

--
-- >>> readValidateWith ("Expected Int, received: " ++) `traverse` splitOn "," "1,2,3,df,6,3,gbud,$$,8,9,19"
-- Failure ["Expected Int, received: df", "Expected Int, received: gbud", "Expected Int, received: $$"]
--
readValidateWith  :: (Element txt ~ Char, MonoFoldable txt, Read a)
                  => (txt -> e)
                  -> txt
                  -> Validation [e] a
readValidateWith fe = first (:[]) . eitherToValidation . readNoteWith fe

-- --
-- -- >>> readValidateExpect "Int" `traverse` splitOn "," "1,2,3,df,6,3,gbud,$$,8,9,19"
-- -- Failure ["Expected Int, received: df", "Expected Int, received: gbud", "Expected Int, received: $$"]
-- --
readValidateExpect  :: (IsString e, IsSequence e, Element e ~ Char, Element txt ~ Char, MonoFoldable txt, Read a)
                    => e
                    -> txt
                    -> Validation [e] a
readValidateExpect expect = first (:[]) . eitherToValidation . readNoteExpect expect
