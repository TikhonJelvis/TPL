{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import TPL.Test.Parse

main = $quickCheckAll

tests = [prop_parseNull, prop_parseId, prop_parseNum, prop_parseStr, 
         prop_parseBool, prop_parseOp, prop_parseBlock, prop_parseIf,
         prop_parseIfElse]