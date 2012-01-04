module TPL.Native where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error

import TPL.Coerce
import TPL.Error
import TPL.Value

eagerNatives = [("length", len), ("+", numOp (+)), ("-", numOp (-)),
                ("*", numOp (*)), ("/", numOp div), ("|", liftOp (||)), 
                ("&", liftOp (&&)), ("=", eqOp (==)), ("/=", eqOp (/=)),
                ("><", strOp (++)), (":", cons), ("!", index), ("..", range),
                ("head", \ _ [ls] -> return $ tplHead ls),
                ("tail", \ _ [ls] -> return $ tplTail ls), ("open", open)]

len :: TPLOperation
len _ [List ls] = return . Number $ length ls
len _ _         = return $ Number 1
        
cons :: TPLOperation
cons env [String head, String tail] = return . String $ head ++ tail
cons env [val, tail@(String _)]     = do str <- liftThrows $ toString val
                                         cons env [str, tail]
cons env [head@(String _), List []] = return head
cons env [head, List tail]          = return . List $ head : tail
cons env [head, tail]               = return . List $ head : [tail]

index :: TPLOperation
index env [List list, Number i]  = return $ list !! i
index env [List list, val]       = liftThrows $ (list !!) <$> (extract <=< toNumber) val
index env [String str, Number i] = return . String $ [str !! i]
index env [String str, val]      = liftThrows $ (String . return . (str !!)) <$> (extract <=< toNumber) val
index env [val, i]               = index env [(List [val]), i]

range :: TPLOperation
range env [Number start, Number end] = return . List $ map Number [start..end]
range env args                       = mapM (liftThrows . toNumber) args >>= range env

open :: TPLOperation
open env args = do args   <- mapM (liftThrows <<< extract <=< toString) args
                   result <- concat <$> mapM (liftIO . readFile) args
                   return $ String result

tplHead (List ls)     = head ls
tplHead (String str)  = String $ [head str]
tplHead val           = val
tplTail (List ls)     = List $ tail ls 
tplTail (String str)  = String $ tail str
tplTail _             = List []
        