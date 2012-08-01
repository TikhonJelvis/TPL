module TPL.Eval where

import Control.Monad.Error           (throwError, liftIO, runErrorT)

import Data.Functor                  ((<$>))

import Text.ParserCombinators.Parsec (parse)

import TPL.Env                       (getEnvRef, getPrecs)
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
  where go (Id name) = do res <- getEnvRef name env
                          case res of
                            Function closure [] body -> eval closure body
                            val                      -> return val
        go e@Expression{} = (`normalize` e) <$> liftIO getPrecs >>= evalExpr
          where evalExpr (Expression (λ : args)) = eval env λ >>= (`apply` args)
                apply fn args = throw $ TypeMismatch "function" fn
        