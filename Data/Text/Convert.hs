-- TODO: Perhaps Data.Text.Encoding.Extra?
module Data.Text.Convert where 

import Prelude 
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE 
import qualified Data.ByteString.Lazy as BL

toLazyByteStringUtf8 :: T.Text -> BL.ByteString
toLazyByteStringUtf8 = TLE.encodeUtf8 . TL.fromStrict