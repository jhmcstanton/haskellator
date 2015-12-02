

module Main where

import MIPs.Types

import Language.Haskellator.Parse

import Control.Applicative
import Data.Word


-- TO get far enough to use these the first 6 bits will already be parsed (OP code field)
constructR :: Parser InstrWidth MInstruction 
constructR = R <$> getField 5 <*> getField 5 <*> getField 5 <*> getField 5 <*> (getField 6 >>= return . toFunc)

{-
constructI :: Parser InstrWidth MInstruction
constructI = I <$> getField 

constructJ :: Parser InstrWidth M
constructJ = J <$> 
-}
