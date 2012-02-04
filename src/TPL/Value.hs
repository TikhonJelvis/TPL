module TPL.Value (TPLValue(..), Env, nullEnv) where

import Data.IORef
import Data.List

type Env = IORef [(String, IORef TPLValue)]

nullEnv = newIORef []

data TPLValue = Null
              | Id String
              | Number Integer
              | String String
              | Boolean Bool
              | Operator String
              | List [TPLValue]
              | Expression [TPLValue]
              | Sequence [TPLValue]
              | Lambda [TPLValue] TPLValue
              | Function Env [TPLValue] TPLValue 
              | Native String
              | If TPLValue TPLValue TPLValue deriving (Eq)
                
showSeq vals = intercalate " " $ map show vals

instance Show TPLValue where
  show (Null)                   = "null"
  show (Id id)                  = id
  show (String str)             = str
  show (Number int)             = show int
  show (Operator name)          = name
  show (Boolean bool)           = if bool then "true" else "false"
  show (List vals)              = show vals
  show (Expression vals)        = "(" ++ showSeq vals ++ ")"
  show (Sequence vals)          = "{\n" ++ (unlines $ map show vals) ++ "}"
  show (Native name)            = "[<native> " ++ name ++ "]"
  show (Function e [] body)     = "$(" ++ show body ++ ")"
  show (Lambda [] body)         = "$(" ++ show body ++ ")"
  show (Function e params body) = showFun params body
  show (Lambda params body)     = showFun params body
  show (If condition consequent alternate) = "{?if " ++ show condition ++
                                             " then " ++ show consequent ++
                                             " else " ++ show alternate ++ "?}"

showFun :: [TPLValue] -> TPLValue -> String
showFun params body = "λ " ++ showSeq params ++ " → " ++ show body