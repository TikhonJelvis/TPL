{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances#-}
module TPL.Coerce (TPLOperation, 
                   liftOp, numOp, eqOp, strOp, eqNumOp,
                   toBool, toNumber, toString, 
                   extract, pack) where

import Control.Monad.Error

import TPL.Error
import TPL.Value

type TPLOperation = Env -> [TPLValue] -> IOThrowsError TPLValue

-- Takes a function on TPLValues and makes it coerce to the given type.
class Extractable a where extract :: TPLValue -> ThrowsError a
class Packable a where pack :: a -> TPLValue
  
instance Extractable Integer where
  extract (Number n) = return n
  extract num = toNumber num >>= extract
instance Packable Integer where pack = Number
                            
instance Extractable [Char] where extract = return . show
instance Packable [Char] where pack = String

instance Extractable Bool where 
  extract (Boolean bool)  = return bool
  extract val             = toBool val >>= extract
instance Packable Bool where pack = Boolean
                             
instance Extractable a => Extractable [a] where
  extract (List vals) = sequence $ map extract vals
  extract value       = sequence [extract value]
instance Packable a => Packable [a] where pack = List . map pack

liftOp :: (Extractable a, Extractable b, Packable c) => (a -> b -> c) -> TPLOperation
liftOp op = \ _ [a, b] ->
  do av <- liftThrows $ extract a
     bv <- liftThrows $ extract b
     return . pack $ op av bv
numOp   = liftOp :: (Integer -> Integer -> Integer) -> TPLOperation
eqOp    = liftOp :: (String -> String -> Bool) -> TPLOperation
eqNumOp = liftOp :: (Integer -> Integer -> Bool) -> TPLOperation
strOp   = liftOp :: (String -> String -> String) -> TPLOperation

toNumber :: TPLValue -> ThrowsError TPLValue
toNumber num@(Number{}) = return num
toNumber (String str)   = return . Number $ read str
toNumber (List [])      = throwError . TypeMismatch "Number" . show $ List []
toNumber (List (val:_)) = toNumber val
toNumber (Boolean bool) = return . Number $ if bool then 1 else 0
toNumber val            = throwError . TypeMismatch "Number" $ show val

toString :: TPLValue -> ThrowsError TPLValue
toString = liftM String . extract

toBool :: TPLValue -> ThrowsError TPLValue
toBool b@(Boolean{}) = return b
toBool Null          = return $ Boolean False
toBool (Number 0)    = return $ Boolean False
toBool (List [])     = return $ Boolean False
toBool _             = return $ Boolean True