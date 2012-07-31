module TPL.Eval where

import Data.Functor ((<$>))

import Text.ParserCombinators.Parsec (parse)

import TPL.Env                       (getEnvRef, getPrecs)
import TPL.Error                     (Result, ErrorType(..), Error(..), pushTrace)
import TPL.Parse                     (expressions)
import TPL.Syntax                    (normalize)
import TPL.Value

readExpr :: String -> Either Error Term
readExpr expr = case parse expressions "TPL" expr of
  Left err  -> Left . Error [] $ Parser err
  Right val -> Right val

eval :: EnvRef -> Term -> IO Result
eval env expr = do res <- go expr
                   return $ case res of
                     Left err -> Left $ pushTrace err expr 
                     res      -> res
  where go (Id name) = do res <- getEnvRef name env
                          case res of
                            Right (Function closure [] body) -> eval closure body
                            val@Right{}                      -> return val
                            err@Left{}                       -> return err
        go e@Expression{} = (`normalize` e) <$> getPrecs >>= evalExpr env
          where evalExpr (Expression (fn@Function{} : args)) = apply fn args
                
        apply = undefined