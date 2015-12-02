{-# LANGUAGE DataKinds, TypeFamilies, GeneralizedNewtypeDeriving #-}


module Language.Haskellator.Parse.Type where

import GHC.TypeLits
import Data.Word
import Control.Monad.State

class (KnownNat n) => FieldSize n where
    type WordType n
    data Width n
    wordToWidth :: WordType n -> Width n
    widthToWord :: Width n    -> WordType n  

