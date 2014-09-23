module Data.ByteString.Extra where

import Prelude
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL

fromStrict :: B.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . (:[])

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
