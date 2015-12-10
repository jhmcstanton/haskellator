

import qualified Data.ByteString.Lazy as BS
import           Data.Bits
import           Data.Word
import           Data.Binary

rInstructionFile = "simple_r_instructions.bin"

-- this stores as Big Endian D:
simpleRWrites = [BS.appendFile rInstructionFile . encode $ rs `shiftL` 21 + rt `shiftL` 16 + rd `shiftL` 11 + func 
                | rs     <- [0..2 :: Word32]
                , rt     <- [3..8 :: Word32]
                , let rd =  rs + rt
                , func   <- [0x20, 0x18 ::Word32]]
