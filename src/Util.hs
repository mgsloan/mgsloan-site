module Util where

import GHC.Stack
import qualified Data.Map as M
import Data.Maybe

(!) :: (Show k, Ord k, HasCallStack) => M.Map k a -> k -> a
mp ! k = fromMaybe (error ("Key " ++ show k ++ " is missing.")) $ M.lookup k mp
