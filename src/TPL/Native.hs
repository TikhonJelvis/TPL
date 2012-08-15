{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, IncoherentInstances #-}
module TPL.Native where

import Control.Arrow (first, second)
import Data.Functor  ((<$>))

import TPL.Value

class Extract a where extract :: Value -> a

class Pack a where pack :: a -> Value

instance Pack Number where pack = Number
instance Extract Number where
  extract (Number n) = fromInteger n
  extract _          = error "extract: not a number!"
  
instance Pack Int where pack = Number . toInteger
                           
instance Pack String where pack = String
instance Extract String where
  extract (String s) = s
  extract _          = error "extract: not a string!"

instance Pack Bool where pack = Bool
instance Extract Bool where
  extract (Bool b) = b
  extract _        = error "extract: not a bool!"
                         
instance Pack a => Pack [a] where pack = List . map pack
