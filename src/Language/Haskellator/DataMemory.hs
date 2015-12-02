module Language.Haskellator.DataMemory
    ( DataMemory
    , dmWrite
    , dmRead
    , dmModify
    )
  where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- TODO: Compare this with an unpacked array
type DataMemory a = IntMap a

dmWrite :: Integral i => i -> a -> DataMemory a -> DataMemory a
dmWrite index = IntMap.insert (fromIntegral index)

dmRead :: Integral i => i -> DataMemory a -> Maybe a
dmRead index = IntMap.lookup (fromIntegral index)

dmModify :: Integral i => (a -> a) -> i -> DataMemory a -> DataMemory a
dmModify f index = IntMap.adjust f (fromIntegral index)

