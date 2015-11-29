{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeFamilies, 
             FlexibleInstances, TypeOperators, AllowAmbiguousTypes, 
             UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Language.Haskellator.Parse where

import Data.Binary
import Data.Bits
import Data.Proxy
import GHC.TypeLits
import Data.Word


class (KnownNat n) => FieldSize n where
    type WordType n
    data Width n
    wordToWidth :: WordType n -> Width n
    widthToWord :: Width n    -> WordType n  
 

parseField :: forall k n m. ((k + n) ~ m, FieldSize k, FieldSize n, FieldSize m, 
              Integral (WordType k), Integral (WordType n), Integral (WordType m), 
              Bits (WordType k), Bits (WordType n), Bits (WordType m)) =>
              Width m -> (Width n, Width k)
parseField c = (wordToWidth . fromIntegral . shiftR (widthToWord c) . fromIntegral $ (kVal - nVal), 
                wordToWidth . fromIntegral . shiftR (widthToWord c) . fromIntegral $ nVal) 
  where
    kVal = natVal (Proxy :: Proxy m)
    nVal = natVal (Proxy :: Proxy n)

instance FieldSize 1 where
  type WordType 1        = Word8
  data Width 1           = Width1 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width1
  widthToWord (Width1 n) = n

instance FieldSize 2 where
  type WordType 2        = Word8
  data Width 2           = Width2 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width2
  widthToWord (Width2 n) = n

instance FieldSize 3 where
  type WordType 3        = Word8
  data Width 3           = Width3 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width3
  widthToWord (Width3 n) = n

instance FieldSize 4 where
  type WordType 4        = Word8
  data Width 4           = Width4 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width4
  widthToWord (Width4 n) = n

instance FieldSize 5 where
  type WordType 5        = Word8
  data Width 5           = Width5 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width5
  widthToWord (Width5 n) = n

instance FieldSize 6 where
  type WordType 6        = Word8
  data Width 6           = Width6 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width6
  widthToWord (Width6 n) = n

instance FieldSize 7 where
  type WordType 7        = Word8
  data Width 7           = Width7 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width7
  widthToWord (Width7 n) = n

instance FieldSize 8 where
  type WordType 8        = Word8
  data Width 8           = Width8 !Word8 deriving (Eq, Show, Ord)
  wordToWidth            = Width8
  widthToWord (Width8 n) = n

instance FieldSize 9 where
  type WordType 9        = Word16
  data Width 9           = Width9 !Word16 deriving (Eq, Show, Ord)
  wordToWidth            = Width9
  widthToWord (Width9 n) = n


w :: Width 8
w = Width8 131

w' :: (Width 3, Width 5)
w' = parseField w

--instance (KnownNat n, n <= 8) => FieldSize n where
  --type Width n = Word8 
  
{-instance (9 <= n, n <= 16) => FieldSize n where
  type Width n = Word16

instance (17 <= n, n <= 32) => FieldSize n where
  type Width n = Word32

instance (33 <= n, n <= 64) => FieldSize n where
  type Width n = Word64
-}
