module TPL.Eval where

import Control.Applicative           ((<$>), (<*>))
import Control.Arrow                 (second)
import Control.Monad.Error           (throwError, liftIO, runErrorT, mapM, foldM)

import Data.IORef                    (newIORef)
import Data.Map                      (fromList)

import Text.ParserCombinators.Parsec (parse)

import TPL.Env                       (getEnvRef, getPrecs, bindEnvRef)
import TPL.Error                     (Result)
import qualified TPL.Error as Err 
import TPL.Parse                     (expressions)
import TPL.Syntax                    (normalize, squash)
import TPL.Value

readExpr :: String -> Either Err.Error Term
readExpr expr = case parse expressions "TPL" expr of
  Left err  -> Left . Err.Error [] $ Err.Parser err
  Right val -> Right val
  
evalString :: EnvRef -> String -> IO String
evalString env inp = showRes <$> runErrorT (Err.liftEither (readExpr inp) >>= eval env)
  where showRes (Left err)  = Err.showErrorStack err
        showRes (Right res) = displayVal res
                        
eval :: EnvRef -> Term -> Result Value
eval env expr = do res <- liftIO . runErrorT . go $ squash expr
                   case res of
                     Left err  -> throwError $ Err.pushTrace err expr 
                     Right res -> return res
  where go NullLiteral         = return Null
        go (NumericLiteral n)  = return $ Number n
        go (StringLiteral s)   = return $ String s
        go (BoolLiteral b)     = return $ Bool b
        go name@Id{}           = do res <- getEnvRef (Symbol $ display name) env
                                    case res of
                                      Function closure [] body -> eval closure body
                                      val                      -> return val
        go e@Expression{}      = (`normalize` e) <$> liftIO getPrecs >>= evalExpr
          where evalExpr (Expression [])         = return Null
                evalExpr (Expression [term])     = eval env term
                evalExpr (Expression (λ : args)) = eval env λ >>= \ fn -> foldM apply fn args
                evalExpr expr                    = eval env expr

                apply fn@(Function _ [] _) _ = Err.throw $ Err.TooManyArguments fn
                apply fn@(Function cl [p] body) arg = getArgEnv p arg cl >>= (`eval` body)
                apply fn@(Function cl (p:ps) body) arg = do cl' <- getArgEnv p arg cl
                                                            return $ Function cl' ps body
                apply fn _ = Err.throw $ Err.TypeMismatch "function" fn

                getArgEnv (Id name) arg oldEnv = do val <- eval env arg
                                                    liftIO $ bindEnvRef [(Symbol name, val)] oldEnv
                getArgEnv (Lambda [] (Id name)) arg oldEnv =
                  liftIO $ bindEnvRef [(Symbol name, defer env arg)] oldEnv
        
        go (ListLiteral terms) = List <$> mapM (eval env) terms
        go (Lambda args body)  = return $ Function env args body
        go (Block [])          = return Null
        go (Block terms)       = last <$> mapM (eval env) terms
        go (ObjectLiteral bindings) = Object . EnvRef <$> newRef
          where newRef = bindings' >>= liftIO . newIORef . fromList
                bindings' = mapM evalBinding bindings
                evalBinding (key, val) = (,) <$> eval env key <*> eval env val 

defer :: EnvRef -> Term -> Value
defer env term = Function env [] term