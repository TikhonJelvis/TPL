{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, IncoherentInstances #-}
module TPL.Native where

import Control.Arrow (first, second)
import Data.Functor  ((<$>))

import TPL.Value

class Extract a where extract :: Term -> a

class Pack a where pack :: a -> Value

instance Pack Number where pack = Number
instance Extract Number where
  extract (NumericLiteral n) = fromInteger n
  extract _                  = error "extract: not a number!"
  
instance Pack Int where pack = Number . toInteger
                           
instance Pack String where pack = String
instance Extract String where
  extract (StringLiteral s) = s
  extract _                 = error "extract: not a string!"

instance Pack Bool where pack = Bool
instance Extract Bool where
  extract (BoolLiteral b) = b
  extract _               = error "extract: not a bool!"
                         
instance Pack a => Pack [a] where pack = List . map pack

instance (Extract a, Pack b) => Pack (a -> b) where
  pack f = Native . NativeOpr $ \ x -> return . pack . f $ extract x

natives :: [(Value, Value)]
natives = first Symbol <$> (convert math ++ convert comp)
  where convert :: Pack a => [(String, a)] -> [(String, Value)]
        convert = (second pack <$>)
        math :: [(String, Integer -> Integer -> Integer)]
        math = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]
        comp :: [(String, Integer -> Integer -> Bool)]
        comp = [(">", (>)), ("<", (<)), ("<=", (<=)), (">=", (>=))]