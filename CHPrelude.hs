module CHPrelude (module CHPrelude) where

import ClassyPrelude      as CHPrelude hiding ( head                          -- These are unsafe
                                              , zip, zip3, zipWith, zipWith3  -- These are somewhat unsafe (import explicitly)
                                              , readFile, writeFile
                                              , sequence                      -- Replaced by more general functions
                                              , first, second
                                              )
import Text.Read          as CHPrelude (lex, readsPrec, readEither)
import Control.Error.Safe as CHPrelude
import Data.Bifunctor     as CHPrelude
