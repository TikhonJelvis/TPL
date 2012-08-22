module Main where

import System.Environment

import TPL.Run

version :: String
version = "0.3.0"

welcome :: String
welcome = "Welcome to TPL version " ++ version ++ "\nType \"--quit\" to quit."

main :: IO ()
main = getArgs >>= go
  where go [] = putStrLn welcome >> repl
        go ls = mapM_ runFile ls
