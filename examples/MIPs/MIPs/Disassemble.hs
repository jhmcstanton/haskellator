module MIPs.Disassemble
  (disassemble
  ,showDisassembled
  )
  where

import           MIPs.Types

import           Data.Char (toLower)

disassemble :: [MInstruction] -> String
disassemble = unlines . map showDisassembled

-- TODO: Implement instructions that involve shamt
showDisassembled :: MInstruction -> String
showDisassembled (R d s t shamt f) =
  fName ++ " " ++ commaSeparate (map showRegister [d, s, t])
  where
    fName    = map toLower (show f)
    hasShamt = False -- For now

commaSeparate :: [String] -> String
commaSeparate []     = ""
commaSeparate [x]    = x
commaSeparate (x:xs) = x ++ ", " ++ commaSeparate xs

showRegister :: Address -> String
showRegister r = "$r" ++ show r

