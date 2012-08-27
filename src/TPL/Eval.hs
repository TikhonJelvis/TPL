{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}
module TPL.Eval where

import Control.Applicative           ((<$>), (<*>), (<|>))
import Control.Arrow                 (first, second)
import Control.Monad.Error           (throwError, liftIO, runErrorT, foldM)

import Data.IORef                    (newIORef)
import Data.Map                      (fromList)

import Text.ParserCombinators.Parsec (parse)

import TPL.Env                       (getEnv, getEnvRef, getPrecs, bindEnvRef, defineEnvRef, setEnvRef)
import qualified TPL.Error as Err 
import TPL.Native
import TPL.Parse                     (program)
import TPL.Pattern                   (unify)
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
                        
        -- TODO: Add support for deffered parents.
getFrom :: EnvRef -> Value -> Result Value
getFrom env (String "*current*") = return $ Object env
getFrom env name = getEnvRef env name <|> inherited <|> custom
  where inherited = do parent <- getEnvRef env (String "*parent*")
                       case parent of Object ref   -> getFrom ref name
                                      v            -> Err.throw $ TypeMismatch "object" v
        custom = do getter <- getEnvRef env (String "*get*") <|> Err.throw (UndefinedVariable name)
                    case getter of f@Function{} -> applyVal env f name
                                   v            -> Err.throw $ TypeMismatch "function" v

customSet :: EnvRef -> Value -> Value -> Result Value
customSet env name value = do setter <- getEnvRef env (String "*set*") <|> Err.throw (UndefinedVariable name)
                              case setter of f@Function{} -> foldM (applyVal env) setter [name, value]
                                             v            -> Err.throw $ TypeMismatch "function" v

setIn :: EnvRef -> Value -> Value -> Result Value
setIn env name value = setEnvRef env name value <|> inherited <|> customSet env name value
  where inherited = do parent <- getEnvRef env (String "*parent*")
                       case parent of Object ref -> setIn ref name value
                                      v          -> Err.throw $ TypeMismatch "object" v

defineIn :: EnvRef -> Value -> Value -> Result Value
defineIn env name value = customSet env name value <|> defineEnvRef env name value

bindObj :: [(Value, Value)] -> EnvRef -> Result EnvRef
bindObj new base = makeEnvRef $ (String "*parent*", Object base) : new

makeEnvRef :: [(Value, Value)] -> Result EnvRef
makeEnvRef = fmap EnvRef . liftIO . newIORef . fromList

eval :: EnvRef -> Term -> Result Value
eval env expr = do res <- liftIO . runErrorT . go $ squash expr
                   case res of
                     Left err  -> throwError $ Err.pushTrace err expr
                     Right r   -> return r
  where go Operator{}          = error "go: cannot eval operator!"
        go NullLiteral         = return Null
        go (NumericLiteral n)  = return $ Number n
        go (StringLiteral s)   = return $ String s
        go (BoolLiteral b)     = return $ Bool b
        go name@Id{}           = do res <- getFrom env . String $ display name
                                    case res of Function closure [] body -> eval closure body
                                                val                      -> return val
        go e@Expression{}      = (`normalize` e) <$> getPrecs env >>= evalExpr
          where evalExpr (Expression [])         = return Null
                evalExpr (Expression [term])     = eval env term
                evalExpr (Expression (λ : args)) = eval env λ >>= \ fn -> foldM (apply env) fn args
                evalExpr expression              = eval env expression
        go (ListLiteral terms) = List <$> mapM (eval env) terms
        go (Lambda args body)  = return $ Function env args body
        go (Block [])          = return Null
        go (Block terms)       = last <$> mapM (eval env) terms
        go (ObjectLiteral bindings) = Object <$> newRef
          where newRef = bindings' >>= makeEnvRef
                bindings' = mapM evalBinding bindings
                evalBinding (Expression (f:args), body) = evalBinding (f, Lambda args body)
                evalBinding (key, val)                  = (,) <$> toString key <*> eval env val
                toString (Id x) = return $ String x
                toString (StringLiteral s) = return $ String s
                toString v = Err.throw $ BadIdentifier v

apply :: EnvRef -> Value -> Term -> Result Value
apply _ fn@(Function _ [] _) _          = Err.throw $ Err.TooManyArguments fn
apply env (Function cl [p] body) arg    = getArgEnv env p arg cl >>= (`eval` body)
apply env (Function cl (p:ps) body) arg = do cl' <- getArgEnv env p arg cl
                                             return $ Function cl' ps body
apply env (Native (NativeOpr opr)) arg  = opr env arg
apply _ fn _                            = Err.throw $ Err.TypeMismatch "function" fn

getArgEnv :: EnvRef -> Term -> Term -> EnvRef -> Result EnvRef
getArgEnv env (Lambda [] n) arg oldEnv = getArgEnv env n (Lambda [] arg) oldEnv
getArgEnv env name arg oldEnv = do val <- eval env arg
                                   let context = (String "*context*", Object env)
                                   bindObj (context : (first String <$> unify name val)) oldEnv

applyVal :: EnvRef -> Value -> Value -> Result Value
applyVal env (Function cl [p] body) val    = newEnv cl p val >>= (`eval` body)
applyVal env (Function cl (p:ps) body) val = (\ e -> Function e ps body) <$> newEnv cl p val

newEnv :: EnvRef -> Term -> Value -> Result EnvRef
newEnv env name value = bindObj (first String <$> unify name value) env
                 
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
        rest = [("=",            pack eqOp),
                (":",            pack ((:) :: Value -> [Value] -> [Value])),
                ("><",           pack ((++) :: String -> String -> String)),
                ("substr",       pack $ \ s i j -> drop i $ take j (s :: String)),
                ("typeof",       pack showType),
                ("if",           pack if'), 
                ("puts",         pack putStrLn),
                ("open",         pack readFile),
                ("toString",     pack displayVal),
                ("exprToString", pack exprToString),
                (":=",           pack $ execOnId defineIn),
                ("<-",           pack $ execOnId setIn),
                (".",            pack $ \ obj env (Id x) -> eval env obj >>= getObjId x),
                ("get",          pack $ \ env term -> eval env term >>= getFrom env),
                ("set",          pack $ \ env term value -> eval env term >>= flip (setIn env) value),
                ("define",       pack $ \ env term value -> eval env term >>= flip (defineIn env) value),
                ("getObj",       pack getFrom),
                ("setObj",       pack setIn),
                ("defineObj",    pack defineIn),
                ("loadObj",      pack $ \ env path -> liftIO (readFile path) >>= Err.liftEither . readExpr >>= eval env),
                ("with",         pack with)]
          where eqOp :: Value -> Value -> Value
                eqOp a b = pack $ a == b
                if' (Bool res) env a b = if res then eval env a else eval env b
                if' v _ _ _            = Err.throw $ TypeMismatch "boolean" v
                execOnId fn env inp rval = exec $ simplify inp
                  where simplify (Expression terms) = Expression $ terms >>= flattenExprs
                        simplify x                  = x
                        flattenExprs (Expression e) = e >>= flattenExprs
                        flattenExprs e              = [e]
                        exec (Id x) = eval env rval >>= fn env (String x)
                        exec (Expression (Id ".":obj:Id name:args)) =
                          eval env obj >>= go
                          where go (Object ref)
                                  | null args = eval env rval >>= fn ref (String name)
                                  | otherwise = eval env (Lambda args rval) >>= fn ref (String name)
                                go v            = Err.throw $ TypeMismatch "object" v
                        exec (Expression (fname@Id{}:args)) = execOnId fn env fname $ Lambda args rval
                        exec v                              = Err.throw $ BadIdentifier v
                with (Object ref) (Function _ args body) = return $ Function ref args body
                with Object{} f                          = Err.throw $ TypeMismatch "function" f
                with o _                                 = Err.throw $ TypeMismatch "object" o
                exprToString env (Id x) = getFrom env (String x) >>= displayDeferred
                exprToString env term = eval env term >>= displayDeferred
                displayDeferred (Function _ [] e) = return . String $ display e
                displayDeferred v                 = Err.throw $ TypeMismatch "deferred expression" v
                getObjId name (Object ref) = getFrom ref (String name)
                getObjId _ v               = Err.throw $ TypeMismatch "object" v
                
baseEnv :: IO EnvRef
baseEnv = nullEnv >>= bindEnvRef natives