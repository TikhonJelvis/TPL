module Main where

import System.Environment

import TPL.Run

version :: String
version = "0.3.0"

welcome :: String
welcome = "Welcome to TPL version " ++ version ++ "\nType \"--quit\" to quit."

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn welcome >> repl
            ls -> mapM_ runFile ls