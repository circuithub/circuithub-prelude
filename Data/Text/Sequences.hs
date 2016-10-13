module Data.Text.Sequences where

import           ClassyPrelude hiding (splitAt, tail, zip)
import           Data.List     (findIndex)
import           Data.Text     hiding (findIndex)

--splitSeq :: (Char -> Char -> Bool) -> Text -> [Text]
--splitSeq = error "TODO"

-- | A span of characters which satisfy some sequential predicate and the remainder
spanSeq :: (Char -> Char -> Bool) -> Text -> (Text, Text)
spanSeq f t = (fromMaybe $ const (t, Data.Text.empty)) (fmap (splitAt . (+1)) $ findIndex (not . uncurry f) $ zip t $ tail t) t

-- | Breaks a text once a 2-character sequence is found that doesn't match the predicate
breakSeq :: (Char -> Char -> Bool) -> Text -> (Text, Text)
breakSeq f t = (fromMaybe $ const (t, Data.Text.empty)) (fmap (splitAt . (+1)) $ findIndex (uncurry f) $ zip t $ tail t) t
