module TPL.Native (eagerNatives, operatorPrecedences, precedenceOf) where

import Control.Applicative
import Control.Arrow ((<<<))
import Control.Monad.Error

import Data.List

import TPL.Coerce
import TPL.Env
import TPL.Error
import TPL.Value

eagerNatives :: [(String, TPLOperation)]
eagerNatives = [("+", numOp (+)), ("-", numOp (-)), ("*", numOp (*)),
                ("/", numOp div), ("=", eqOp (==)), ("/=", eqOp (/=)),
                (">", eqNumOp (>)), ("><", strOp (++)), (":", cons),
                ("open", open), ("_print", _print), ("substr", substr),
                ("length", len), ("_if", ifTPL)]

precedenceOf :: Env -> String -> IOThrowsError Integer
precedenceOf env op = getPrecedence env op >>= liftThrows . extract

operatorPrecedences :: [Integer]
operatorPrecedences = [11,10..0]

cons :: TPLOperation
cons _ [first, List rest] = return . List $ first : rest
cons _ [first, rest]      = return . List $ first : [rest]
cons _ expr               = throwError $ BadNativeCall "(:)" expr

open :: TPLOperation
open _ args = do res    <- mapM (liftThrows <<< extract <=< toString) args
                 result <- concat <$> mapM (liftIO . readFile) res
                 return $ String result

_print :: TPLOperation
_print _ args = mapM (liftIO . putStrLn . show) args >> return Null

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
substr _ expr = throwError $ BadNativeCall "substr" expr
                                 
len :: TPLOperation
len _ [String str] = return . Number $ genericLength str
len _ [List ls]    = return . Number $ genericLength ls

len env [expr]     = do str <- liftThrows $ toString expr
                        len env [str]
len _ expr         = throwError $ BadNativeCall "len" expr
                        
ifTPL :: TPLOperation
ifTPL _ [Boolean condition, consequent, alternate] = 
  return $ if condition then consequent else alternate
ifTPL env [condition, consequent, alternate] =
  do cond <- liftThrows $ toBool condition
     ifTPL env [cond, consequent, alternate]
ifTPL _ expr = throwError $ BadNativeCall "_if" expr
