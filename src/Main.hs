module Main where

import System.Environment

import TPL.Run

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> repl
            ls -> mapM_ runFile ls