{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module TPL.Native where

import           Control.Applicative ((<$>))
import           Control.Arrow       (first, second)
import           Control.Monad.Error (liftIO)

import           TPL.Env             (bindEnvRef, getEnvRef)
import           TPL.Error           (liftEither, throw)
import           TPL.Eval            (defineIn, eval, getFrom, readExpr, setIn)
import           TPL.Pack            (Extract, Pack, extract, native, pack)
import           TPL.Pattern         (unify)
import           TPL.Value           (EnvRef, ErrorType (..), Result, Term (..),
                                      Value (..), display, displayVal, nullEnv,
                                      showType)

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
                ("_if",           pack if'),
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
                ("loadObj",      pack $ \ env path -> liftIO (readFile path) >>= liftEither . readExpr path >>= eval env),
                ("with",         pack with)]
          where eqOp :: Value -> Value -> Value
                eqOp a b = pack $ a == b
                if' (Bool res) env a b = if res then eval env a else eval env b
                if' v _ _ _            = throw $ TypeMismatch "boolean" v
                execOnId fn env inp rval = exec $ simplify inp
                  where simplify (Expression terms) = Expression $ terms >>= flattenExprs
                        simplify x                  = x
                        flattenExprs (Expression e) = e >>= flattenExprs
                        flattenExprs e              = [e]
                        exec (Id x) = eval env rval >>= fn env (String x)
                        exec names@ListLiteral{} =
                          do v <- eval env rval
                             let bs = first String <$> unify names v
                             mapM_ (uncurry $ fn env) bs
                             return v
                        exec (Expression (Id ".":obj:Id name:args)) =
                          eval env obj >>= go
                          where go (Object ref)
                                  | null args = eval env rval >>= fn ref (String name)
                                  | otherwise = eval env (Lambda args rval) >>= fn ref (String name)
                                go v            = throw $ TypeMismatch "object" v
                        exec (Expression (fname@Id{}:args)) = execOnId fn env fname $ Lambda args rval
                        exec v                              = throw $ BadIdentifier v
                with ref env (Id x) = getEnvRef env (String x) >>= with' ref
                with ref env expr   = eval env expr >>= with' ref
                with' (Object ref) (Function _ args body) = return $ Function ref args body
                with' Object{} f                          = throw $ TypeMismatch "function" f
                with' o _                                 = throw $ TypeMismatch "object" o
                exprToString env (Id x) = getFrom env (String x) >>= displayDeferred
                exprToString env term = eval env term >>= displayDeferred
                displayDeferred (Function _ [] e) = return . String $ display e
                displayDeferred v                 = throw $ TypeMismatch "deferred expression" v
                getObjId name (Object ref) = getFrom ref (String name)
                getObjId _ v               = throw $ TypeMismatch "object" v

baseEnv :: IO EnvRef
baseEnv = nullEnv >>= bindEnvRef natives
