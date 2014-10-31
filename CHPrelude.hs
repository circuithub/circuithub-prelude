module CHPrelude (module CHPrelude) where

import ClassyPrelude        as CHPrelude hiding ( head                          -- These are unsafe
                                                , zip, zip3, zipWith, zipWith3  -- These are somewhat unsafe (import explicitly)
                                                , readFile, writeFile
                                                , sequence                      -- Replaced by more general functions
                                                , first, second
                                                )
import Text.Read            as CHPrelude ( lex
                                         , readsPrec
                                         , readEither
                                         )
import Data.Bifunctor       as CHPrelude
import Data.Traversable     as CHPrelude ( traverse
                                         , sequence
                                         )
import Control.Monad        as CHPrelude ( zipWithM
                                         , msum
                                         , foldM_
                                         )
import Control.Error.Safe   as CHPrelude
