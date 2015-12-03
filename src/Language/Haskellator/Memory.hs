module Language.Haskellator.Memory
    ( Memory
    , memWrite
    , memRead
    , memModify
    )
  where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type RegisterFile = Memory
type DataMemory   = Memory

-- TODO: Compare this with an unpacked array
type Memory a = IntMap a

memWrite :: Integral i => i -> a -> Memory a -> Memory a
memWrite index = IntMap.insert (fromIntegral index)

memRead :: Integral i => i -> Memory a -> Maybe a
memRead index = IntMap.lookup (fromIntegral index)

memModify :: Integral i => (a -> a) -> i -> Memory a -> Memory a
memModify f index = IntMap.adjust f (fromIntegral index)

