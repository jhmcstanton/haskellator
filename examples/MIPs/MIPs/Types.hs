{-# LANGUAGE KindSignatures, GADTs #-}

module MIPs.Types where

import Data.Word


data MInstruction :: * where
  R :: Address -> Address -> Address -> Shamt -> Func -> MInstruction
  I :: OP -> Address      -> Address -> Immediate     -> MInstruction
  J :: OP -> JumpAddress  -> MInstruction


data OP :: * where -- all MIPS opcodes emulated
  -- R-Type (OP field = 0)
  R_op :: OP -- hate this name

  -- J-Type (OP Field = 1 or 2)
  J_op :: OP -- this one too
  JAL  :: OP

  -- I-Type (OP Field >= 3) -- if *all* I-types have constructors in order then we can use toEnum here when decoding
  BEQ  :: OP
  BNE  :: OP  
  deriving (Eq, Ord, Show, Enum)


data Func :: * where -- all R-Type Functions emulated
{-  SLL  :: FUNC -- if *all* functions have constructors in order then we can use toEnum here when decoding
  SRL  :: FUNC
  SRA  :: FUNC
  SLLV :: FUNC
  SRLV :: FUNC
  SRAV :: FUNC
  JR   :: FUNC
  JALR :: FUNC --two forms (JALR $rd $rs and JALR $rs)-}
  ADD  :: Func
  MULT :: Func
  AND  :: Func
  OR   :: Func
  deriving (Eq, Ord, Show, Enum)


-- hopefully this is replaceable with toEnum later
toOP :: Word8 -> OP
toOP n | n == 0 = R_op
       | n == 2 = J_op
       | n == 3 = JAL 
       | n == 4 = BEQ
       | n == 5 = BNE

-- hopefully this is easily replaceable with toEnum later
toFunc :: Word8 -> Func 
toFunc n | n == 0x20 = ADD --should probably check these..
         | n == 0x18 = MULT
         | n == 0x88 = AND
         | n == 0x89 = OR

type InstrWidth  = Word32
type Address     = Word8
type Shamt       = Word8
type Immediate   = Word16
type JumpAddress = Word32
