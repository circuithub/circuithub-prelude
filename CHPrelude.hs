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
import Data.Monoid          as CHPrelude ( Monoid (mappend, mempty, mconcat)
                                         )
import Data.Bifunctor       as CHPrelude
import Data.Traversable     as CHPrelude ( traverse
                                         , sequence
                                         )
import Control.Applicative  as CHPrelude ( Applicative
                                         , pure
                                         , (<$>)
                                         , (<*>)
                                         , (<|>)
                                         , (<*)
                                         )
import Control.Arrow        as CHPrelude ( (&&&)
                                         )
import Control.Monad        as CHPrelude ( (<=<)
                                         , join
                                         , zipWithM
                                         , when
                                         , unless
                                         , void
                                         , msum
                                         , mplus
                                         , foldM_
                                         )
import Control.Error.Safe   as CHPrelude
