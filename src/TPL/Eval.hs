{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}
module TPL.Eval where

import Control.Applicative           ((<$>), (<*>), (<$))
import Control.Arrow                 (first, second)
import Control.Monad.Error           (throwError, liftIO, runErrorT, foldM, (=<<))

import Data.IORef                    (newIORef, readIORef)
import Data.Map                      (fromList, union)

import Text.ParserCombinators.Parsec (parse)

import TPL.Env                       (getEnvRef, getPrecs, bindEnvRef, defineEnvRef, setEnvRef)
import qualified TPL.Error as Err 
import TPL.Native
import TPL.Parse                     (program)
import TPL.Syntax                    (normalize, squash)
import TPL.Value as Err

readExpr :: String -> Either Err.Error Term
readExpr expr = case parse program "TPL" expr of
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
                     Right r   -> return r
  where go Operator{}          = error "eval -> go: cannot eval operator!"
        go NullLiteral         = return Null
        go (NumericLiteral n)  = return $ Number n
        go (StringLiteral s)   = return $ String s
        go (BoolLiteral b)     = return $ Bool b
        go name@Id{}           = do res <- getEnvRef env . String $ display name
                                    case res of
                                      Function closure [] body -> eval closure body
                                      val                      -> return val
        go e@Expression{}      = (`normalize` e) <$> liftIO getPrecs >>= evalExpr
          where evalExpr (Expression [])         = return Null
                evalExpr (Expression [term])     = eval env term
                evalExpr (Expression (λ : args)) = eval env λ >>= \ fn -> foldM apply fn args
                evalExpr expression              = eval env expression

                apply fn@(Function _ [] _) _        = Err.throw $ Err.TooManyArguments fn
                apply (Function cl [p] body) arg    = getArgEnv p arg cl >>= (`eval` body)
                apply (Function cl (p:ps) body) arg = do cl' <- getArgEnv p arg cl
                                                         return $ Function cl' ps body
                apply (Native (NativeOpr opr)) arg  = opr env arg
                apply fn _                          = Err.throw $ Err.TypeMismatch "function" fn

                getArgEnv (Id name) arg oldEnv = do val <- eval env arg
                                                    liftIO $ bindEnvRef [(String name, val)] oldEnv
                getArgEnv (Lambda [] (Id name)) arg oldEnv =
                  liftIO $ bindEnvRef [(String name, defer env arg)] oldEnv
                getArgEnv _ _ _ = error "getArgEnv: names can only be ids or lambdas!"
        
        go (ListLiteral terms) = List <$> mapM (eval env) terms
        go (Lambda args body)  = return $ Function env args body
        go (Block [])          = return Null
        go (Block terms)       = last <$> mapM (eval env) terms
        go (ObjectLiteral bindings) = Object . EnvRef <$> newRef
          where newRef = bindings' >>= liftIO . newIORef . fromList
                bindings' = mapM evalBinding bindings
                evalBinding (key, val) = (,) <$> toString key <*> eval env val
                toString (Id x) = return $ String x
                toString (StringLiteral s) = return $ String s
                toString v = Err.throw $ BadIdentifier v

defer :: EnvRef -> Term -> Value
defer env term = Function env [] term

                 -- Native functions:
instance (Extract a, Pack b) => Pack (a -> Result b) where
  pack f = native $ \ env x -> pack <$> (eval env x >>= extract >>= f)

instance (Extract a, Pack b) => Pack (a -> IO b) where pack f = pack (liftIO . f :: a -> Result b)

instance (Extract a, Pack b) => Pack (a -> b) where pack f = pack (return . f :: a -> Result b)

natives :: [(Value, Value)]
natives = first String <$> (convert math ++ convert comp ++ rest)
  where convert :: Pack a => [(String, a)] -> [(String, Value)]
        convert = (second pack <$>)
        math :: [(String, Integer -> Integer -> Integer)]
        math = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]
        comp :: [(String, Integer -> Integer -> Bool)]
        comp = [(">", (>)), ("<", (<)), ("<=", (<=)), (">=", (>=))]
        rest = [("=", pack eqOp),
                ("><", pack ((++) :: String -> String -> String)),
                ("substr", pack $ \ s i j -> drop i $ take j (s :: String)),
                ("typeof", pack showType),
                ("_if", pack if'), 
                ("puts", pack putStrLn),
                ("open", pack readFile),
                ("toString", pack displayVal),
                ("exprToString", pack exprToString),
                (":=", pack $ execOnId defineEnvRef),
                ("<-", pack $ execOnId setEnvRef),
                ("#", pack $ \ obj env (Id x) -> eval env obj >>= getObjId x),
                ("get", pack $ \ env term -> eval env term >>= getEnvRef env),
                ("set", pack $ \ env term value -> eval env term >>= flip (setEnvRef env) value),
                ("define", pack $ \ env term value -> eval env term >>= flip (defineEnvRef env) value),
                ("getObj", pack getEnvRef),
                ("setObj", pack setEnvRef),
                ("defineObj", pack defineEnvRef),
                ("with", pack with)]
          where eqOp :: Value -> Value -> Value
                eqOp a b = pack $ a == b
                if' (Bool res) env a b = if res then eval env a else eval env b
                if' v _ _ _            = Err.throw $ TypeMismatch "boolean" v
                displayExp (Function _ [] exp) = return . String $ display exp
                displayExp v = Err.throw $ TypeMismatch "function" v
                execOnId fn env inp rval = exec $ simplify inp
                  where simplify (Expression ((Expression ls):rest)) = Expression $ ls ++ rest
                        simplify x                                   = x
                        exec (Id x)                           = eval env rval >>= fn env (String x)
                        exec (Expression ((Id "#"):obj:rest)) = eval env obj >>= go
                          where go (Object ref) = execOnId fn ref (squash $ Expression rest) rval
                                go v            = Err.throw $ TypeMismatch "object" v
                        exec (Expression (fname@Id{}:args)) = execOnId fn env fname $ Lambda args rval
                        exec v                              = Err.throw $ BadIdentifier v
                with (Object ref) (Function _ args body) = return $ Function ref args body
                with Object{} f                          = Err.throw $ TypeMismatch "function" f
                with o _                                 = Err.throw $ TypeMismatch "object" o
                exprToString env (Id x) = getEnvRef env (String x) >>= displayDeferred
                exprToString env term = eval env term >>= displayDeferred
                displayDeferred (Function _ [] e) = return . String $ display e
                displayDeferred v                 = Err.throw $ TypeMismatch "deferred expression" v
                getObjId name (Object ref) = getEnvRef ref (String name)
                getObjId name v            = Err.throw $ TypeMismatch "object" v
                
baseEnv :: IO EnvRef
baseEnv = nullEnv >>= bindEnvRef natives