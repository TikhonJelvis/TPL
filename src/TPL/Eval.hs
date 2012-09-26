module TPL.Eval where

import           Control.Applicative           ((<$>), (<*>), (<|>))
import           Control.Monad.Error           (foldM, liftIO, runErrorT,
                                                throwError)

import           Data.IORef                    (newIORef)
import           Data.List                     (minimumBy)
import           Data.Map                      (fromList)
import           Data.Ord                      (comparing)

import           Text.ParserCombinators.Parsec (parse)

import           TPL.Env                       (defineEnvRef, getEnvRef,
                                                setEnvRef)
import qualified TPL.Error                     as Err
import           TPL.Parse                     (program)
import           TPL.Pattern                   (unify)
import           TPL.Syntax                    (isOp, processOp, squash)
import           TPL.Value                     as Err

readExpr :: FilePath -> String -> Either Err.Error Term
readExpr source expr = case parse program source expr of
  Left err  -> Left . Err.Error [] $ Err.Parser err
  Right val -> Right val

evalString :: FilePath -> EnvRef -> String -> IO String
evalString source env inp = showRes <$> runErrorT (Err.liftEither (readExpr source inp) >>= eval env)
  where showRes (Left err)  = Err.showErrorStack err
        showRes (Right res) = displayVal res

        -- TODO: Add support for deferred parents.
getFrom :: EnvRef -> Value -> Result Value
getFrom env (String "*current*") = return $ Object env
getFrom env name = getEnvRef env name <|> inherited <|> custom
  where inherited = do parent <- getEnvRef env (String "*parent*")
                       case parent of Object ref   -> getFrom ref name
                                      v            -> Err.throw $ TypeMismatch "object" v
        custom = do getter <- getEnvRef env (String "*get*") <|> Err.throw (UndefinedVariable name)
                    case getter of f@Function{} -> applyVal f name
                                   v            -> Err.throw $ TypeMismatch "function" v

customSet :: EnvRef -> Value -> Value -> Result Value
customSet env name value = do setter <- getEnvRef env (String "*set*") <|> Err.throw (UndefinedVariable name)
                              case setter of Function{} -> foldM applyVal setter [name, value]
                                             v          -> Err.throw $ TypeMismatch "function" v

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

processOperators :: EnvRef -> Term -> Result Term
processOperators env expr@(Expression body) =
  do precs <- mapM getPrec $ operators body
     return $ case zip precs $ operators body of
       [] ->  expr
       ls -> processOp (snd $ minimumBy (comparing fst) ls) expr
  where operators = map (\ (Operator o) -> o) . filter isOp
        getPrec s = do val <- getFrom env (String "*precs*") >>= extract >>= (`getFrom` String s)
                       case val of Number n -> return n
                                   v        -> Err.throw $ TypeMismatch "number" v
        extract (Object ref) = return ref
        extract v            = Err.throw $ TypeMismatch "object" v
processOperators _ term = return term

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
        go e@Expression{} = processOperators env e >>= evalExpr
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
                                   bindObj (context : unify name val) oldEnv

applyVal :: Value -> Value -> Result Value
applyVal (Function cl [p] body) val    = newEnv cl p val >>= (`eval` body)
applyVal (Function cl (p:ps) body) val = (\ e -> Function e ps body) <$> newEnv cl p val
applyVal fn _                          = Err.throw $ Err.TypeMismatch "function" fn

newEnv :: EnvRef -> Term -> Value -> Result EnvRef
newEnv env name value = bindObj (unify name value) env

defer :: EnvRef -> Term -> Value
defer env term = Function env [] term
