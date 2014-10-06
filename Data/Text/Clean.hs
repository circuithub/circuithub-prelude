module Data.Text.Clean where

import ClassyPrelude hiding (take, null, drop, dropWhile, span)
import Data.Text

dropParens :: Text -> Text
dropParens t =  let mi = findIndex (== '(') t
                in case mi of 
                  (Just i) -> take i t `append` (dropParens . dropUntilClosing . drop (i + 1)) t
                  Nothing  -> t
  where 
    dropUntilClosing s = maybe s (\j -> drop (j + 1) s) (findIndex (== ')') s)

stripTags :: Text -> Text
stripTags t | null t  = t
            | otherwise = uncurry append $ (stripTags . drop 1 . dropWhile (/= '>') . drop 1) `second` span (/= '<') t
