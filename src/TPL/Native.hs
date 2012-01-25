module TPL.Native where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error

import Data.List

import TPL.Coerce
import TPL.Error
import TPL.Value

eagerNatives = [("+", numOp (+)), ("-", numOp (-)), ("*", numOp (*)),
                ("/", numOp div), ("=", eqOp (==)), ("/=", eqOp (/=)),
                (">", eqNumOp (>)), ("><", strOp (++)), (":", cons),
                ("open", open), ("print", printTPL), ("substr", substr),
                ("length", len)]

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

substr :: TPLOperation
substr _ [String str, Number start, Number end] =
  let s = fromIntegral start
      e = fromIntegral end in
  return . String . take (e - s) $ drop s str
substr env [str, start, end] = result >>= substr env
  where result = liftThrows $ do str'   <- toString str
                                 start' <- toNumber start
                                 end'   <- toNumber end
                                 return $ [str', start', end']
                                 
len :: TPLOperation
len _ [String str] = return . Number $ genericLength str
len _ [List ls]    = return . Number $ genericLength ls
len e [exp]        = do str <- liftThrows $ toString exp
                        len e [str]
                         