{-# LANGUAGE KindSignatures, GADTs #-}

module MIPS.Types where

import Data.Word

data MInstruction :: * where
  R :: Address -> Address -> Address -> Shamt -> Func -> MInstruction
  I :: OP -> Address      -> Address -> Immediate -> MInstruction
  J :: OP -> JumpAddress  -> MInstruction

data OP :: * where -- all MIPS opcodes emulated
  -- R-Type
  R_op :: OP

  -- J-Type
  J_op :: OP
  JAL  :: OP

  -- I-Type
  BEQ  :: OP
  BNE  :: OP  


data Func :: * where -- all R-Type Functions emulated
  ADD  :: Func
  MULT :: Func
  AND  :: Func
  OR   :: Func

type Address     = Word8
type Shamt       = Word8
type Immediate   = Word16
type JumpAddress = Word32
