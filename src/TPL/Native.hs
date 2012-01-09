module TPL.Native where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error

import Data.List

import TPL.Coerce
import TPL.Error
import TPL.Value

eagerNatives = [("+", numOp (+)), ("-", numOp (-)),
                ("*", numOp (*)), ("/", numOp div), ("|", liftOp (||)), 
                ("&", liftOp (&&)), ("=", eqOp (==)), ("/=", eqOp (/=)),
                (">", eqNumOp (>)),
                ("><", strOp (++)), (":", cons),
                ("open", open), ("print", printTPL)]

cons :: TPLOperation
cons _ [String head, String tail]   = return . String $ head ++ tail
cons env [val, tail@(String _)]     = do str <- liftThrows $ toString val
                                         cons env [str, tail]
cons _ [head@(String _), List []] = return head
cons _ [head, List tail]          = return . List $ head : tail
cons _ [head, tail]               = return . List $ head : [tail]

open :: TPLOperation
open _ args = do args   <- mapM (liftThrows <<< extract <=< toString) args
                 result <- concat <$> mapM (liftIO . readFile) args
                 return $ String result
                 
printTPL :: TPLOperation
printTPL _ args = mapM (liftIO . putStrLn . show) args >> return Null
                                      