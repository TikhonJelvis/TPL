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

import           Text.Printf

import           TPL.Run

main = do samples <- runSamples
          if samples then runQuickCheck else do putStr "FAILED"
                                                exitFailure

runSamples = do files <- getCurrentDirectory >>= getDirectoryContents
                let scripts = sort $ filter (isSuffixOf ".tpl") files
                let out     = sort $ filter (isSuffixOf ".out") files
                results <- zipWithM runTest scripts out
                printf "Passed %d/%d tests.\n" (length $ filter id results) (length results)
                return $ and results
  where runTest file out = do (result, ()) <- capture $ runFile file
                              expected    <- readFile out
                              let failed = result /= expected
                              when failed $ do putStrLn $ "Failed " ++ file
                                               putStr   $ "Expected:\n" ++ expected
                                               putStr   $ "Got:\n" ++ result
                                               putStrLn $ replicate 80 '-'
                              return $ not failed

runQuickCheck = return () -- $quickCheckAll

-- tests = [prop_parseNull, prop_parseId, prop_parseNum, prop_parseStr,
--          prop_parseBool, prop_parseOp, prop_parseBlock, prop_parseIf,
--          prop_parseIfElse]
