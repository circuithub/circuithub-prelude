module Data.List.Split.Extra
  ( splitPlaces
  ) where

import Prelude
import qualified Data.List.Split as LS (splitPlaces)
import Data.Text (Text, pack)

-- | This replaces 'Data.List.Split.splitPlaces' which has an issue where trailing 0 length lists aren't preserved
--   when the end of a list has been reached
--
--   TODO: this could also be called splitPlacesExact (but splitPlaces is a bit unsafe, so just shadowing it for now)
--
splitPlaces :: Integral a => [a] -> [b] -> Either Text [[b]]
splitPlaces ns xs = if fromIntegral (length xs) /= sum ns
                    then Left $ pack "splitPlaces: list is too short to be split by places"
                    else let xs' = LS.splitPlaces ns xs
                         in Right $ xs' ++ replicate (length ns - length xs') []
