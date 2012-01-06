module Main where

import System.Environment

import TPL.Run

welcome = "Welcome to TPL. Type \"quit\" to quit."

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn welcome >> repl
            ls -> mapM_ runFile ls