{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Haskellator.Machine.Type
  ( Machine (..)
  , VM

  , readPC
  , incrPC
  , writePC

  , readDM
  , writeDM
  , modifyDM
  )
  where

import           Language.Haskellator.Machine.Memory

import           GHC.TypeLits
import           GHC.Prim (Proxy#, proxy#)

import           Control.Monad.State

import           Lens.Micro.TH
import           Lens.Micro.Mtl

data Machine d r (pcRegNum :: Nat)
  = Machine
      { _dataMemory   :: DataMemory d
      , _registerFile :: RegisterFile r
      }

makeLenses ''Machine

type VM d r (pcRegNum :: Nat) = State (Machine d r pcRegNum)

-- PC operations --
readPC :: forall d r pcRegNum. KnownNat pcRegNum => VM d r pcRegNum r
readPC = memRead pcIx <$> gets _registerFile
  where
    pcIx = natVal' (proxy# :: Proxy# pcRegNum)

incrPC :: forall d r pcRegNum.
          (Num r, KnownNat pcRegNum)
            => VM d r pcRegNum ()
incrPC = registerFile %= memModify (+1) pcIx
  where
    pcIx = natVal' (proxy# :: Proxy# pcRegNum)

writePC :: forall d r pcRegNum.
         (Num r, KnownNat pcRegNum)
           => r -> VM d r pcRegNum ()
writePC newPC = registerFile %= memWrite pcIx newPC
  where
    pcIx = natVal' (proxy# :: Proxy# pcRegNum)

-- Data memory operations --
readDM :: (Integral i, Show i) => i -> VM d r pcRegNum d
readDM i = memRead i <$> gets _dataMemory

writeDM :: Integral i => i -> d -> VM d r pcRegNum ()
writeDM i a = dataMemory %= memWrite i a

modifyDM :: Integral i => (d -> d) -> i -> VM d r pcRegNum ()
modifyDM f i = dataMemory %= memModify f i

