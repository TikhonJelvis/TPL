module TPL.Syntax (normalize) where

import Control.Applicative
import Control.Monad

import Data.Function
import Data.List

import TPL.Env
import TPL.Error
import TPL.Native
import TPL.Pattern
import TPL.Value

handleInfix :: Env -> TPLValue -> IOThrowsError TPLValue
handleInfix env (Expression exp) =
  squash . Expression <$> (foldl1 (.) handleAll $ return exp')
  where exp' = map (squash . Expression) $ groupBy ((==) `on` isOp) exp
        handleAll = map ((=<<) . handle) operatorPrecedences
        handle precedence vals@(left : op@(Operator opStr) : right : more) = precedenceOf env opStr >>= handleOp op . fromIntegral
          where handleOp op actualPrecdence
                  | actualPrecdence  == precedence = 
                    handle precedence $ Expression [left, op, right] : more
                  | otherwise = (left:) <$> handle precedence (op:right:more)
        handle precedence (left:more) = fmap (left:) $ handle precedence more
        handle precedence val         = return val
handleInfix _ value = return value

section :: [TPLValue] -> TPLValue
section = Lambda [Id "α"] . Expression

desugar :: TPLValue -> TPLValue
desugar (Expression [op@(Operator{}), right])  = section [Id "α", op, desugar right]
desugar (Expression [left, op@(Operator{})])   = section [desugar left, op, Id "α"]
desugar (Expression [a, Operator op, b])       = Expression [Id op, desugar a, desugar b]
desugar (Expression exp)                       = Expression $ map desugar exp
desugar (Operator op)                          = Id op
desugar val                                    = val

isOp Operator{} = True
isOp _          = False

normalize = (liftM desugar .) . handleInfix