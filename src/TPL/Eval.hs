module TPL.Eval where

import Control.Arrow                 (second)
import Control.Monad.Error           (throwError, liftIO, runErrorT, mapM, foldM)

import Data.Functor                  ((<$>))
import Data.IORef                    (newIORef)
import Data.Map                      (fromList)

import Text.ParserCombinators.Parsec (parse)

import TPL.Env                       (getEnvRef, getPrecs, bindEnvRef)
import TPL.Error                     (Result, ErrorType(..), Error(..), pushTrace, throw)
import TPL.Parse                     (expressions)
import TPL.Syntax                    (normalize)
import TPL.Value

readExpr :: String -> Either Error Term
readExpr expr = case parse expressions "TPL" expr of
  Left err  -> Left . Error [] $ Parser err
  Right val -> Right val

eval :: EnvRef -> Term -> Result Value
eval env expr = do res <- liftIO . runErrorT $ go expr
                   case res of
                     Left err  -> throwError $ pushTrace err expr 
                     Right res -> return res
  where go NullLiteral         = return Null
        go (NumericLiteral n)  = return $ Number n
        go (StringLiteral s)   = return $ String s
        go (BoolLiteral b)     = return $ Bool b
        go name@Id{}           = do res <- getEnvRef name env
                                    case res of
                                      Function closure [] body -> eval closure body
                                      val                      -> return val
        go e@Expression{}      = (`normalize` e) <$> liftIO getPrecs >>= evalExpr
          where evalExpr (Expression (λ : args)) = eval env λ >>= \ fn -> foldM apply fn args

                apply fn@(Function _ [] _) _ = throw $ TooManyArguments fn
                apply fn@(Function cl [p] body) arg = do cl' <- getArgEnv arg cl 
                                                         eval cl' body
                apply fn@(Function cl (p:ps) body) arg = do cl' <- getArgEnv arg cl
                                                            return $ Function cl' ps body
                apply fn _ = throw $ TypeMismatch "function" fn

                getArgEnv arg oldEnv = do val <- eval env arg
                                          liftIO $ bindEnvRef [(Id "*α", val)] oldEnv
        
        go (ListLiteral terms) = List <$> mapM (eval env) terms 
        go (Lambda args body)  = return $ Function env args body
        go (Block [])          = return Null
        go (Block terms)       = last <$> mapM (eval env) terms
        go (ObjectLiteral bindings) = Object . EnvRef <$> newRef
          where newRef = liftIO . newIORef $ fromList bindings
                bindings = undefined
