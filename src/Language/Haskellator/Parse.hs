{-# LANGUAGE DataKinds, TypeFamilies, 
             FlexibleInstances, TypeOperators, 
             ScopedTypeVariables, FlexibleContexts,
             BangPatterns, RankNTypes #-}

module Language.Haskellator.Parse where

import Language.Haskellator.Parse.Type

import Data.Binary hiding (get, put)
import Data.Bits
import Data.Proxy
import GHC.TypeLits
import Data.Word
import Control.Monad.State
import Control.Monad.Reader


type Parser w r = ReaderT Int (State (w, Int)) r

runParser :: (Num w, FiniteBits w) => Parser w r -> w -> (r, (w, Int))
runParser parser instruction = runState (runReaderT parser (finiteBitSize instruction)) (instruction, 0)

getField :: (Integral w, Integral r, FiniteBits w, FiniteBits r) => Int -> Parser w r 
getField width = do
  (word, numUsed) <- get
  size            <- ask
  put (word .&. (2 ^ (size - (numUsed + width)) - 1), numUsed + width)
  return . fromIntegral $ (word .&. ((2 ^ width - 1) `shiftL` (size - numUsed - width))) `shiftR` (size - numUsed - width) 





----------------------------------
-- Neat Type stuff below
----------------------------------

parseField :: forall k n m. ((k + n) ~ m, FieldSize k, FieldSize n, FieldSize m, 
              Integral (WordType k), Integral (WordType n), Integral (WordType m), 
              1 <= m, 1 <= n, -- disallow parsing of empty instructions and ensure that at least 1 bit is parsed 
              Bits (WordType k), Bits (WordType n), Bits (WordType m)) =>
              Width m -> (Width n, Width k)
parseField c = (wordToWidth . fromIntegral . (.&. leftMask ) . shiftR (widthToWord c) $ kVal, 
                wordToWidth . fromIntegral . (.&. rightMask) . widthToWord $ c) 
  where
    nVal       = natVal (Proxy :: Proxy n)
    kVal       = fromIntegral $ natVal (Proxy :: Proxy k)
    leftMask   = (2 ^ nVal) - 1
    rightMask  = (2 ^ kVal) - 1

instance FieldSize 0 where 
  type WordType 0  = Word8 -- ideally this would be (), but it doesn't play niceley with parseField
  data Width 0     = Width0  deriving (Show, Eq, Ord)
  wordToWidth _    = Width0
  widthToWord _    = 0 

instance FieldSize 1 where
  type WordType 1 = Word8
  data Width 1 = Width1 !(WordType 1) deriving(Show, Eq, Ord)
  wordToWidth = Width1
  widthToWord (Width1 n) = n
instance FieldSize 2 where
  type WordType 2 = Word8
  data Width 2 = Width2 !(WordType 2) deriving(Show, Eq, Ord)
  wordToWidth = Width2
  widthToWord (Width2 n) = n
instance FieldSize 3 where
  type WordType 3 = Word8
  data Width 3 = Width3 !(WordType 3) deriving(Show, Eq, Ord)
  wordToWidth = Width3
  widthToWord (Width3 n) = n
instance FieldSize 4 where
  type WordType 4 = Word8
  data Width 4 = Width4 !(WordType 4) deriving(Show, Eq, Ord)
  wordToWidth = Width4
  widthToWord (Width4 n) = n
instance FieldSize 5 where
  type WordType 5 = Word8
  data Width 5 = Width5 !(WordType 5) deriving(Show, Eq, Ord)
  wordToWidth = Width5
  widthToWord (Width5 n) = n
instance FieldSize 6 where
  type WordType 6 = Word8
  data Width 6 = Width6 !(WordType 6) deriving(Show, Eq, Ord)
  wordToWidth = Width6
  widthToWord (Width6 n) = n
instance FieldSize 7 where
  type WordType 7 = Word8
  data Width 7 = Width7 !(WordType 7) deriving(Show, Eq, Ord)
  wordToWidth = Width7
  widthToWord (Width7 n) = n
instance FieldSize 8 where
  type WordType 8 = Word8
  data Width 8 = Width8 !(WordType 8) deriving(Show, Eq, Ord)
  wordToWidth = Width8
  widthToWord (Width8 n) = n
instance FieldSize 9 where
  type WordType 9 = Word16
  data Width 9 = Width9 !(WordType 9) deriving(Show, Eq, Ord)
  wordToWidth = Width9
  widthToWord (Width9 n) = n
instance FieldSize 10 where
  type WordType 10 = Word16
  data Width 10 = Width10 !(WordType 10) deriving(Show, Eq, Ord)
  wordToWidth = Width10
  widthToWord (Width10 n) = n
instance FieldSize 11 where
  type WordType 11 = Word16
  data Width 11 = Width11 !(WordType 11) deriving(Show, Eq, Ord)
  wordToWidth = Width11
  widthToWord (Width11 n) = n
instance FieldSize 12 where
  type WordType 12 = Word16
  data Width 12 = Width12 !(WordType 12) deriving(Show, Eq, Ord)
  wordToWidth = Width12
  widthToWord (Width12 n) = n
instance FieldSize 13 where
  type WordType 13 = Word16
  data Width 13 = Width13 !(WordType 13) deriving(Show, Eq, Ord)
  wordToWidth = Width13
  widthToWord (Width13 n) = n
instance FieldSize 14 where
  type WordType 14 = Word16
  data Width 14 = Width14 !(WordType 14) deriving(Show, Eq, Ord)
  wordToWidth = Width14
  widthToWord (Width14 n) = n
instance FieldSize 15 where
  type WordType 15 = Word16
  data Width 15 = Width15 !(WordType 15) deriving(Show, Eq, Ord)
  wordToWidth = Width15
  widthToWord (Width15 n) = n
instance FieldSize 16 where
  type WordType 16 = Word16
  data Width 16 = Width16 !(WordType 16) deriving(Show, Eq, Ord)
  wordToWidth = Width16
  widthToWord (Width16 n) = n
instance FieldSize 17 where
  type WordType 17 = Word32
  data Width 17 = Width17 !(WordType 17) deriving(Show, Eq, Ord)
  wordToWidth = Width17
  widthToWord (Width17 n) = n
instance FieldSize 18 where
  type WordType 18 = Word32
  data Width 18 = Width18 !(WordType 18) deriving(Show, Eq, Ord)
  wordToWidth = Width18
  widthToWord (Width18 n) = n
instance FieldSize 19 where
  type WordType 19 = Word32
  data Width 19 = Width19 !(WordType 19) deriving(Show, Eq, Ord)
  wordToWidth = Width19
  widthToWord (Width19 n) = n
instance FieldSize 20 where
  type WordType 20 = Word32
  data Width 20 = Width20 !(WordType 20) deriving(Show, Eq, Ord)
  wordToWidth = Width20
  widthToWord (Width20 n) = n
instance FieldSize 21 where
  type WordType 21 = Word32
  data Width 21 = Width21 !(WordType 21) deriving(Show, Eq, Ord)
  wordToWidth = Width21
  widthToWord (Width21 n) = n
instance FieldSize 22 where
  type WordType 22 = Word32
  data Width 22 = Width22 !(WordType 22) deriving(Show, Eq, Ord)
  wordToWidth = Width22
  widthToWord (Width22 n) = n
instance FieldSize 23 where
  type WordType 23 = Word32
  data Width 23 = Width23 !(WordType 23) deriving(Show, Eq, Ord)
  wordToWidth = Width23
  widthToWord (Width23 n) = n
instance FieldSize 24 where
  type WordType 24 = Word32
  data Width 24 = Width24 !(WordType 24) deriving(Show, Eq, Ord)
  wordToWidth = Width24
  widthToWord (Width24 n) = n
instance FieldSize 25 where
  type WordType 25 = Word32
  data Width 25 = Width25 !(WordType 25) deriving(Show, Eq, Ord)
  wordToWidth = Width25
  widthToWord (Width25 n) = n
instance FieldSize 26 where
  type WordType 26 = Word32
  data Width 26 = Width26 !(WordType 26) deriving(Show, Eq, Ord)
  wordToWidth = Width26
  widthToWord (Width26 n) = n
instance FieldSize 27 where
  type WordType 27 = Word32
  data Width 27 = Width27 !(WordType 27) deriving(Show, Eq, Ord)
  wordToWidth = Width27
  widthToWord (Width27 n) = n
instance FieldSize 28 where
  type WordType 28 = Word32
  data Width 28 = Width28 !(WordType 28) deriving(Show, Eq, Ord)
  wordToWidth = Width28
  widthToWord (Width28 n) = n
instance FieldSize 29 where
  type WordType 29 = Word32
  data Width 29 = Width29 !(WordType 29) deriving(Show, Eq, Ord)
  wordToWidth = Width29
  widthToWord (Width29 n) = n
instance FieldSize 30 where
  type WordType 30 = Word32
  data Width 30 = Width30 !(WordType 30) deriving(Show, Eq, Ord)
  wordToWidth = Width30
  widthToWord (Width30 n) = n
instance FieldSize 31 where
  type WordType 31 = Word32
  data Width 31 = Width31 !(WordType 31) deriving(Show, Eq, Ord)
  wordToWidth = Width31
  widthToWord (Width31 n) = n
instance FieldSize 32 where
  type WordType 32 = Word32
  data Width 32 = Width32 !(WordType 32) deriving(Show, Eq, Ord)
  wordToWidth = Width32
  widthToWord (Width32 n) = n
instance FieldSize 33 where
  type WordType 33 = Word64
  data Width 33 = Width33 !(WordType 33) deriving(Show, Eq, Ord)
  wordToWidth = Width33
  widthToWord (Width33 n) = n
instance FieldSize 34 where
  type WordType 34 = Word64
  data Width 34 = Width34 !(WordType 34) deriving(Show, Eq, Ord)
  wordToWidth = Width34
  widthToWord (Width34 n) = n
instance FieldSize 35 where
  type WordType 35 = Word64
  data Width 35 = Width35 !(WordType 35) deriving(Show, Eq, Ord)
  wordToWidth = Width35
  widthToWord (Width35 n) = n
instance FieldSize 36 where
  type WordType 36 = Word64
  data Width 36 = Width36 !(WordType 36) deriving(Show, Eq, Ord)
  wordToWidth = Width36
  widthToWord (Width36 n) = n
instance FieldSize 37 where
  type WordType 37 = Word64
  data Width 37 = Width37 !(WordType 37) deriving(Show, Eq, Ord)
  wordToWidth = Width37
  widthToWord (Width37 n) = n
instance FieldSize 38 where
  type WordType 38 = Word64
  data Width 38 = Width38 !(WordType 38) deriving(Show, Eq, Ord)
  wordToWidth = Width38
  widthToWord (Width38 n) = n
instance FieldSize 39 where
  type WordType 39 = Word64
  data Width 39 = Width39 !(WordType 39) deriving(Show, Eq, Ord)
  wordToWidth = Width39
  widthToWord (Width39 n) = n
instance FieldSize 40 where
  type WordType 40 = Word64
  data Width 40 = Width40 !(WordType 40) deriving(Show, Eq, Ord)
  wordToWidth = Width40
  widthToWord (Width40 n) = n
instance FieldSize 41 where
  type WordType 41 = Word64
  data Width 41 = Width41 !(WordType 41) deriving(Show, Eq, Ord)
  wordToWidth = Width41
  widthToWord (Width41 n) = n
instance FieldSize 42 where
  type WordType 42 = Word64
  data Width 42 = Width42 !(WordType 42) deriving(Show, Eq, Ord)
  wordToWidth = Width42
  widthToWord (Width42 n) = n
instance FieldSize 43 where
  type WordType 43 = Word64
  data Width 43 = Width43 !(WordType 43) deriving(Show, Eq, Ord)
  wordToWidth = Width43
  widthToWord (Width43 n) = n
instance FieldSize 44 where
  type WordType 44 = Word64
  data Width 44 = Width44 !(WordType 44) deriving(Show, Eq, Ord)
  wordToWidth = Width44
  widthToWord (Width44 n) = n
instance FieldSize 45 where
  type WordType 45 = Word64
  data Width 45 = Width45 !(WordType 45) deriving(Show, Eq, Ord)
  wordToWidth = Width45
  widthToWord (Width45 n) = n
instance FieldSize 46 where
  type WordType 46 = Word64
  data Width 46 = Width46 !(WordType 46) deriving(Show, Eq, Ord)
  wordToWidth = Width46
  widthToWord (Width46 n) = n
instance FieldSize 47 where
  type WordType 47 = Word64
  data Width 47 = Width47 !(WordType 47) deriving(Show, Eq, Ord)
  wordToWidth = Width47
  widthToWord (Width47 n) = n
instance FieldSize 48 where
  type WordType 48 = Word64
  data Width 48 = Width48 !(WordType 48) deriving(Show, Eq, Ord)
  wordToWidth = Width48
  widthToWord (Width48 n) = n
instance FieldSize 49 where
  type WordType 49 = Word64
  data Width 49 = Width49 !(WordType 49) deriving(Show, Eq, Ord)
  wordToWidth = Width49
  widthToWord (Width49 n) = n
instance FieldSize 50 where
  type WordType 50 = Word64
  data Width 50 = Width50 !(WordType 50) deriving(Show, Eq, Ord)
  wordToWidth = Width50
  widthToWord (Width50 n) = n
instance FieldSize 51 where
  type WordType 51 = Word64
  data Width 51 = Width51 !(WordType 51) deriving(Show, Eq, Ord)
  wordToWidth = Width51
  widthToWord (Width51 n) = n
instance FieldSize 52 where
  type WordType 52 = Word64
  data Width 52 = Width52 !(WordType 52) deriving(Show, Eq, Ord)
  wordToWidth = Width52
  widthToWord (Width52 n) = n
instance FieldSize 53 where
  type WordType 53 = Word64
  data Width 53 = Width53 !(WordType 53) deriving(Show, Eq, Ord)
  wordToWidth = Width53
  widthToWord (Width53 n) = n
instance FieldSize 54 where
  type WordType 54 = Word64
  data Width 54 = Width54 !(WordType 54) deriving(Show, Eq, Ord)
  wordToWidth = Width54
  widthToWord (Width54 n) = n
instance FieldSize 55 where
  type WordType 55 = Word64
  data Width 55 = Width55 !(WordType 55) deriving(Show, Eq, Ord)
  wordToWidth = Width55
  widthToWord (Width55 n) = n
instance FieldSize 56 where
  type WordType 56 = Word64
  data Width 56 = Width56 !(WordType 56) deriving(Show, Eq, Ord)
  wordToWidth = Width56
  widthToWord (Width56 n) = n
instance FieldSize 57 where
  type WordType 57 = Word64
  data Width 57 = Width57 !(WordType 57) deriving(Show, Eq, Ord)
  wordToWidth = Width57
  widthToWord (Width57 n) = n
instance FieldSize 58 where
  type WordType 58 = Word64
  data Width 58 = Width58 !(WordType 58) deriving(Show, Eq, Ord)
  wordToWidth = Width58
  widthToWord (Width58 n) = n
instance FieldSize 59 where
  type WordType 59 = Word64
  data Width 59 = Width59 !(WordType 59) deriving(Show, Eq, Ord)
  wordToWidth = Width59
  widthToWord (Width59 n) = n
instance FieldSize 60 where
  type WordType 60 = Word64
  data Width 60 = Width60 !(WordType 60) deriving(Show, Eq, Ord)
  wordToWidth = Width60
  widthToWord (Width60 n) = n
instance FieldSize 61 where
  type WordType 61 = Word64
  data Width 61 = Width61 !(WordType 61) deriving(Show, Eq, Ord)
  wordToWidth = Width61
  widthToWord (Width61 n) = n
instance FieldSize 62 where
  type WordType 62 = Word64
  data Width 62 = Width62 !(WordType 62) deriving(Show, Eq, Ord)
  wordToWidth = Width62
  widthToWord (Width62 n) = n
instance FieldSize 63 where
  type WordType 63 = Word64
  data Width 63 = Width63 !(WordType 63) deriving(Show, Eq, Ord)
  wordToWidth = Width63
  widthToWord (Width63 n) = n
instance FieldSize 64 where
  type WordType 64 = Word64
  data Width 64 = Width64 !(WordType 64) deriving(Show, Eq, Ord)
  wordToWidth = Width64
  widthToWord (Width64 n) = n

{-
w :: Width 8
w = Width8 131

w' :: (Width 3, Width 5)
w' = parseField w

-}
