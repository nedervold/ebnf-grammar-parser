module SafeMap
  ( (!)
  ) where

import qualified Data.Map as M
import GHC.Stack
import Text.Printf (printf)

(!) :: (HasCallStack, Ord k, Show k) => M.Map k v -> k -> v
(!) m k =
  case M.lookup k m of
    Nothing -> error $ printf "Map.!: %s is not an element in the map" (show k)
    Just v -> v
