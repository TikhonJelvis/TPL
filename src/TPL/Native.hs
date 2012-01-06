module TPL.Native where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error

import Data.List

import TPL.Coerce
import TPL.Error
import TPL.Value

eagerNatives = [("length", len), ("+", numOp (+)), ("-", numOp (-)),
                ("*", numOp (*)), ("/", numOp div), ("|", liftOp (||)), 
                ("&", liftOp (&&)), ("=", eqOp (==)), ("/=", eqOp (/=)),
                ("><", strOp (++)), (":", cons), ("!", index), ("..", range),
                ("tail", \ _ [ls] -> return $ tplTail ls), ("open", open),
                ("print", printTPL)]
  where tplTail (List ls)     = List $ tail ls 
        tplTail (String str)  = String $ tail str
        tplTail _             = List []
        

len :: TPLOperation
len _ [List ls] = return . Number $ genericLength ls
len _ _         = return $ Number 1
        
cons :: TPLOperation
cons _ [String head, String tail]   = return . String $ head ++ tail
cons env [val, tail@(String _)]     = do str <- liftThrows $ toString val
                                         cons env [str, tail]
cons _ [head@(String _), List []] = return head
cons _ [head, List tail]          = return . List $ head : tail
cons _ [head, tail]               = return . List $ head : [tail]

index :: TPLOperation
index _ [List list, Number i]  = return $ list !! fromInteger i
index _ [List list, val]       = liftThrows $ (list !!) . fromInteger <$> (extract <=< toNumber) val
index _ [String str, Number i] = return . String $ [str !! fromInteger i]
index _ [String str, val]      = liftThrows $ (String . return . (str !!) . fromInteger) <$> (extract <=< toNumber) val
index env [val, i]             = index env [(List [val]), i]

range :: TPLOperation
range _ [Number start, Number end] = return . List $ map Number [start..end]
range env args                     = mapM (liftThrows . toNumber) args >>= range env

open :: TPLOperation
open _ args = do args   <- mapM (liftThrows <<< extract <=< toString) args
                 result <- concat <$> mapM (liftIO . readFile) args
                 return $ String result
                 
printTPL :: TPLOperation
printTPL _ args = mapM (liftIO . putStrLn . show) args >> return Null
                                      