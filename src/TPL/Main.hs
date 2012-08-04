module Main where

import System.Environment

import TPL.Run

version = "0.2.0"

welcome = "Welcome to TPL version " ++ version ++ "\n Type \"quit\" to quit."

main = do args <- getArgs
          case args of
            [] -> putStrLn welcome >> repl
            ls -> mapM_ runFile ls