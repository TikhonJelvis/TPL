{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, IncoherentInstances, UndecidableInstances #-}
module TPL.Native where

import qualified TPL.Error as Err
import TPL.Value

class Extract a where extract :: Value -> Result a

class Pack a where pack :: a -> Value

instance Pack Value where pack = id
instance Extract Value where extract = return 

instance Pack () where pack = const Null

instance Pack Int  where pack = Number . toInteger
instance Extract Int where
  extract (Number n) = return $ fromInteger n
  extract v          = Err.throw $ TypeMismatch "number" v
  
instance Pack Number where pack = Number
instance Extract Number where
  extract (Number n) = return n
  extract v          = Err.throw $ TypeMismatch "number" v

instance Pack String where pack = String
instance Extract String where
  extract (String s) = return s
  extract v          = Err.throw $ TypeMismatch "string" v

instance Pack Bool where pack = Bool
instance Extract Bool where
  extract (Bool b) = return b
  extract v        = Err.throw $ TypeMismatch "boolean" v
  
instance Pack EnvRef where pack = Object
instance Extract EnvRef where
  extract (Object ref) = return $ ref
  extract v            = Err.throw $ TypeMismatch "object" v
                         
instance Pack a => Pack [a] where pack = List . map pack
instance Extract [Value] where
  extract (List vals) = return $ vals
  extract v           = Err.throw $ TypeMismatch "list" v

instance Pack (EnvRef -> Term -> Result Value) where pack = native

instance Pack a => Pack (EnvRef -> Term -> a) where
  pack f = native $ \ env x -> return . pack $ f env x

instance Pack a => Pack (Term -> a) where
  pack f = native $ \ _ x -> return . pack $ f x

instance Pack (Term -> Result Value) where
  pack f = native $ const f

native :: (EnvRef -> Term -> Result Value) -> Value
native = Native . NativeOpr
