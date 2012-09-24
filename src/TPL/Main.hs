module Main where

import           System.Environment

import           Text.Printf        (printf)

import           TPL.Run

version :: String
version = "0.5.0"

welcome :: String
welcome = printf "Welcome to TPL version %s\nType \"--quit\" to quit." version

main :: IO ()
main = getArgs >>= go
  where go [] = putStrLn welcome >> repl
        go ls = mapM_ runFile ls
