module Language.Haskellator.Machine.Memory
    ( RegisterFile
    , DataMemory
    , Memory
    , memWrite
    , memRead
    , memModify
    )
  where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type RegisterFile a = Memory a
type DataMemory   a = Memory a

-- TODO: Compare this with an unpacked array
type Memory a = IntMap a

memWrite :: Integral i => i -> a -> Memory a -> Memory a
memWrite index = IntMap.insert (fromIntegral index)

memRead :: (Integral i, Show i) => i -> Memory a -> a
memRead index mem =
  case IntMap.lookup (fromIntegral index) mem of
    Just a -> a
    _      -> error $ "memRead: Index out of bounds: " ++ show index

memModify :: Integral i => (a -> a) -> i -> Memory a -> Memory a
memModify f index = IntMap.adjust f (fromIntegral index)

