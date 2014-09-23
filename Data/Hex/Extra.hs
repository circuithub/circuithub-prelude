{-# LANGUAGE OverloadedStrings #-}
module Data.Hex.Extra where

import Prelude
import Data.List
import Numeric

showHexFixed :: (Integral a, Show a) => Int -> a -> String
showHexFixed len val =
  padZeros $ showHex val ""
    where padZeros s = if length s >= len then s else padZeros ('0' : s)

fromHex :: (Integral a, Read a) => String -> a 
fromHex val =
  let hexPrefix = "0x" :: String
  in read (if hexPrefix `isPrefixOf` val then val else (hexPrefix ++ val))
