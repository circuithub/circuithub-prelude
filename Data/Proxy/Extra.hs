module Data.Proxy.Extra where

import Data.Proxy

-- | Helper to drill through to a type two levels down
joinProxy :: proxy (proxy' e) -> Proxy e
joinProxy _ = Proxy