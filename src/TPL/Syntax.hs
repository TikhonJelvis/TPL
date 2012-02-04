module TPL.Syntax (normalize) where

import Control.Applicative
import Control.Monad.Error

import Data.Function
import Data.List

import TPL.Env
import TPL.Error
import TPL.Native
import TPL.Pattern
import TPL.Value

normalize :: Env -> TPLValue -> IOThrowsError TPLValue
normalize env = handleInfix >=> desugar
  where handleInfix (Expression exp) =
          squash . Expression <$> (foldl1 (.) handleAll $ return exp')
          where exp' = map (squash . Expression) $ groupBy ((==) `on` isOp) exp
                handleAll = map ((=<<) . handle) operatorPrecedences
                handle precedence vals@(left : op@(Operator opStr) : right : more) =
                  precedenceOf env opStr >>= handleOp op . fromIntegral
                  where handleOp op actualPrecdence
                          | actualPrecdence  == precedence = 
                            handle precedence $ Expression [left, op, right] : more
                          | otherwise = (left:) <$> handle precedence (op:right:more)
                handle precedence (left:more) = fmap (left:) $ handle precedence more
                handle precedence val         = return val
        handleInfix value = return value

        desugar (Expression [op@(Operator{}), right])  = section [Id "α", op, right]
        desugar (Expression [left, op@(Operator{})])   = section [left, op, Id "α"]
        desugar (Expression [a, Operator op, b])       = desugar $ Expression [Id op, a, b]
        desugar (Expression exp)                       = Expression <$> mapM (normalize env) exp
        desugar (List items)                           = List <$> mapM (normalize env) items
        desugar (Operator op)                          = return $ Id op
        desugar val                                    = return val

section :: [TPLValue] -> IOThrowsError TPLValue
section = return . Lambda [Id "α"] . Expression

isOp Operator{} = True
isOp _          = False
