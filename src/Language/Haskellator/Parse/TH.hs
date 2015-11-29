module Language.Haskellator.Parse.TH (mkFSInstance) where

import Language.Haskellator.Parse.Type

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word

mkFSInstance :: Name -> Q [Dec]
mkFSInstance n = fmap (: []) $ makeFSInstance n

makeFSInstance :: Name -> Q Dec
makeFSInstance n = 
  let typeN           = conT n 
      familyName      = mkName "Width" 
      constructorName = mkName $ "Width" ++ nameBase n
      constructor :: ConQ
      constructor     = do 
         Just name <- lookupTypeName wordType
         wordType  <- conT name
         return $ NormalC constructorName [(IsStrict, wordType)]
      dataInst :: Q Dec
      dataInst        = dataInstD (return []) familyName [typeN] [constructor] []
      nameInt         = read $ nameBase n :: Int       
      wordType        = if nameInt <= 8 
                        then "Word8"
                        else if nameInt <= 16
                             then "Word16"
                             else if nameInt <= 32
                                  then "Word32"
                                  else "Word64"
      varName = mkName "x"
  in 
    instanceD (return []) (conT $ mkName "FieldSize") [tySynD (mkName "WordType") [PlainTV n] (conT $ mkName wordType)
                                                      , dataInst
                                                      , funD (mkName "widthToWord") [clause [conP constructorName [(bangP $ varP varName)]] 
                                                         (normalB $ varE varName) []]
                                                      , funD (mkName "wordToWidth") [clause [varP varName]
                                                         (normalB $ appE (varE constructorName) (varE varName)) []]
                                                      ]
