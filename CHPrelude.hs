module CHPrelude (module CHPrelude) where

import ClassyPrelude        as CHPrelude hiding ( head                          -- These are unsafe
                                                , zip, zip3, zipWith, zipWith3  -- These are somewhat unsafe (import explicitly)
                                                , readFile, writeFile
                                                , sequence                      -- Replaced by more general functions
                                                , first, second
                                                , elem, notElem                 -- Deprecated (replaced by oelem, onotElem)
                                                , forM, forM_                   -- Replaced by more general for and for_
                                                )
import Data.Foldable        as CHPrelude ( for_
                                         )
import Data.Bifunctor       as CHPrelude
import Data.Bifunctor.Extra as CHPrelude ( mapBoth
                                         )
import Data.Traversable     as CHPrelude ( sequence
                                         )
import Text.Read            as CHPrelude ( lex
                                         , readsPrec
                                         , readEither
                                         )
import Control.Monad        as CHPrelude ( zipWithM
                                         , msum
                                         , foldM_
                                         )
import Data.Either.Validation.Extra as CHPrelude ( validate
                                                 )
import Control.Error.Safe   as CHPrelude hiding ( tryJust
                                                )
import Control.Error.Util   as CHPrelude ( (?:)
                                         )
