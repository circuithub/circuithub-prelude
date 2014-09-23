module Debug.Trace.Extra where

import Prelude
import Debug.Trace
import Data.Aeson.Types (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy (toStrict)

-- Using similar conventions to Debug.Trace.Helpers
traceIt :: (Show a) => a -> a
traceIt x = trace (show x) x

--traceItPretty :: (Show a) => a -> a
--traceItPretty _x = error "TODO: traceItPretty" -- see pretty

-- TODO: use a pretty toJSON
traceItJSON :: (ToJSON a) => a -> a
--traceItJSON x = error "TODO: traceItJSON" 
traceItJSON x = trace ('\n' : (toString . toStrict $ encodePretty x) ++ "\n") x

--traceWith :: (a -> Text) -> a -> a
