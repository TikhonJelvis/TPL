{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad

import           Data.Functor
import           Data.List

import           System.Directory
import           System.Exit
import           System.IO.Silently

import           Test.QuickCheck
import           Test.QuickCheck.All

import           TPL.Run

main = do samples <- runSamples
          if samples then runQuickCheck else do putStr "FAILED"
                                                exitFailure

runSamples = do files <- getCurrentDirectory >>= getDirectoryContents
                let scripts = sort $ filter (isSuffixOf ".tpl") files
                let out     = sort $ filter (isSuffixOf ".out") files
                and <$> zipWithM runTest scripts out
  where runTest file out = do (result, ()) <- capture $ runFile file
                              return $ result == out

runQuickCheck = return () -- $quickCheckAll

-- tests = [prop_parseNull, prop_parseId, prop_parseNum, prop_parseStr,
--          prop_parseBool, prop_parseOp, prop_parseBlock, prop_parseIf,
--          prop_parseIfElse]
