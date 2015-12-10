{-# LANGUAGE KindSignatures, GADTs #-}

module MIPs.Types where

import Data.Word


data MInstruction :: * where
  R :: Address -> Address -> Address -> Shamt -> Func -> MInstruction
  I :: OP -> Address      -> Address -> Immediate     -> MInstruction
  J :: OP -> JumpAddress  -> MInstruction
  deriving (Show)


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
  SRL   :: FUNC
  SRA   :: FUNC
  SLLV  :: FUNC
  SRLV  :: FUNC
  SRAV  :: FUNC
  JR    :: FUNC
  JALR  :: FUNC --two forms (JALR $rd $rs and JALR $rs)-}
  SLL   :: Func -- 0X00, uses SHAMT
  SRL   :: Func -- 0x02, uses SHAMT
  SRA   :: Func -- 0x03, uses SHAMT
  JR    :: Func -- 0x08 
  MFHI  :: Func -- 0x10
  MFLO  :: Func -- 0x12
  MULT  :: Func -- 0x18
  MULTU :: Func -- 0x19
  DIV   :: Func -- 0x1A
  DIVU  :: Func -- 0x1B
  ADD   :: Func -- 0X20 
  ADDU  :: Func -- 0X21
  SUB   :: Func -- 0x22
  SUBU  :: Func -- 0x23
  AND   :: Func -- 0X24
  OR    :: Func -- 0X25
  XOR   :: Func -- 0X26
  NOR   :: Func -- 0x27
  SLT   :: Func -- 0X2A
  SLTU  :: Func -- 0X2B
  deriving (Eq, Ord, Show, Enum)


-- hopefully this is replaceable with toEnum later
toOP :: Word8 -> OP
toOP n | n == 0 = R_op
       | n == 2 = J_op
       | n == 3 = JAL 
       | n == 4 = BEQ
       | n == 5 = BNE

-- hopefully this is easily replaceable with toEnum later
-- this could also be reorderded to account for common usages (reduce branch checks)
toFunc :: Word8 -> Func  
toFunc n | n == 0x00 = SLL
         | n == 0x02 = SRL
         | n == 0x03 = SRA
         | n == 0x08 = JR
         | n == 0x10 = MFHI
         | n == 0x12 = MFLO
         | n == 0x18 = MULT
         | n == 0x19 = MULTU
         | n == 0x1A = DIV
         | n == 0x1B = DIVU
         | n == 0x20 = ADD
         | n == 0x21 = ADDU
         | n == 0x22 = SUB
         | n == 0x23 = SUBU
         | n == 0x24 = AND
         | n == 0x25 = OR
         | n == 0x26 = XOR
         | n == 0x27 = NOR
         | n == 0x2A = SLT
         | n == 0x2B = SLTU

type InstrWidth  = Word32
type Address     = Word8
type Shamt       = Word8
type Immediate   = Word16
type JumpAddress = Word32
