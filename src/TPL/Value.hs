module TPL.Value (TPLValue(..), Env, nullEnv) where

import Data.IORef
import Data.List
import qualified Data.Map as M

type Env = M.Map String TPLValue

nullEnv :: IO Env
nullEnv = newIORef $ M.null

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
              | Env Env
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
  show (Function _ [] body)     = "$(" ++ show body ++ ")"
  show (Lambda [] body)         = "$(" ++ show body ++ ")"
  show (Function _ params body) = showFun params body
  show (Lambda params body)     = showFun params body
  show (Native name)            = "[<native> " ++ name ++ "]"
  show (Env e)                  = show e

showFun :: [TPLValue] -> TPLValue -> String
showFun params body = "λ " ++ showSeq params ++ " → " ++ show body