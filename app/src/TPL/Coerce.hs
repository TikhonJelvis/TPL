{-# LANGUAGE FlexibleInstances #-}
module TPL.Coerce (TPLOperation, 
                   liftOp, numOp, eqOp, strOp, eqNumOp,
                   toBool, toNumber, toString, 
                   extract, pack) 
       where

import Control.Arrow
import Control.Monad.Error

import TPL.Env
import TPL.Error
import TPL.Value

type TPLOperation = Env -> [TPLValue] -> IOThrowsError TPLValue
type Coercer = TPLValue -> ThrowsError TPLValue

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

liftOp :: (Extractable a, Extractable b, Packable c) => (a -> b -> c) -> TPLOperation
liftOp op = \ env [a, b] ->
  do av <- liftThrows $ extract a
     bv <- liftThrows $ extract b
     return . pack $ op av bv
numOp   = liftOp :: (Integer -> Integer -> Integer) -> TPLOperation
eqOp    = liftOp :: (String -> String -> Bool) -> TPLOperation
eqNumOp = liftOp :: (Integer -> Integer -> Bool) -> TPLOperation
strOp   = liftOp :: (String -> String -> String) -> TPLOperation

toNumber :: Coercer
toNumber num@(Number _) = return num
toNumber (String str)   = return . Number $ read str
toNumber (List [])      = throwError . TypeMismatch "Number" . show $ List []
toNumber (List (val:_)) = toNumber val
toNumber (Boolean bool) = return . Number $ if bool then 1 else 0
toNumber val            = throwError . TypeMismatch "Number" $ show val

toString :: Coercer
toString = liftM String . extract

toBool :: Coercer
toBool b@(Boolean _) = return b
toBool (Null)        = return $ Boolean False
toBool (Number 0)    = return $ Boolean False
toBool (List [])     = return $ Boolean False
toBool val           = return $ Boolean True