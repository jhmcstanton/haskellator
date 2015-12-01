{-# LANGUAGE KindSignatures, GADTs #-}

module MIPS.Type where

import Data.Word

data MInstruction :: * where
  R :: OP -> Address     -> Address -> Address   -> Shamt -> Func -> MInstruction
  I :: OP -> Address     -> Address -> Immediate -> MInstruction
  J :: OP -> JumpAddress -> MInstruction

data OP :: * where -- all MIPS opcodes emulated
  OP :: OP

data Func :: * where -- all R-Type Functions emulated
  Func :: Func

type Address     = Word8
type Shamt       = Word8
type Immediate   = Word16
type JumpAddress = Word32



