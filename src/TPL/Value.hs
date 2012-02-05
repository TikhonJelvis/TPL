module TPL.Value (TPLValue(..), Env, nullEnv) where

import Data.IORef
import Data.List

type Env = IORef [(String, IORef TPLValue)]

nullEnv :: IO Env
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
              | Native String deriving (Eq)
                
showSeq :: Show a => [a] -> String
showSeq vals = intercalate " " $ map show vals

instance Show TPLValue where
  show (Null)                   = "null"
  show (Id name)                = name
  show (String str)             = str
  show (Number int)             = show int
  show (Operator name)          = name
  show (Boolean bool)           = if bool then "true" else "false"
  show (List vals)              = show vals
  show (Expression vals)        = "(" ++ showSeq vals ++ ")"
  show (Sequence vals)          = "{\n" ++ (unlines $ map show vals) ++ "}"
  show (Native name)            = "[<native> " ++ name ++ "]"
  show (Function _ [] body)     = "$(" ++ show body ++ ")"
  show (Lambda [] body)         = "$(" ++ show body ++ ")"
  show (Function _ params body) = showFun params body
  show (Lambda params body)     = showFun params body

showFun :: [TPLValue] -> TPLValue -> String
showFun params body = "λ " ++ showSeq params ++ " → " ++ show body