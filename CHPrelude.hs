module CHPrelude (module CHPrelude) where

import ClassyPrelude      as CHPrelude hiding ( head, init, last, tail        -- These are unsafe
                                              , read
                                              , zip, zip3, zipWith, zipWith3  -- These are somewhat unsafe (import explicitly)
                                              , readFile, writeFile
                                              , sequence                      -- Replaced by more general functions
                                              , first, second
                                              )
import Text.Read          as CHPrelude (Read, lex, readsPrec)
import Control.Error.Safe as CHPrelude
import Data.Bifunctor     as CHPrelude