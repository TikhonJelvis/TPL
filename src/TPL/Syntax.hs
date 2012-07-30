module TPL.Syntax where

import Data.Function (on)
import Data.Functor  ((<$>)) 
import Data.List     (groupBy)
import Data.Maybe    (fromMaybe)

import TPL.Value

defaultPrecedence :: Int
defaultPrecedence = 10

possiblePrecedences :: [Int]
possiblePrecedences = [1..11]

normalize :: [(Term, Int)] -> Term -> Term
normalize precs = handleInfix . desugar
  where handleInfix (Expression expr) = Expression $ foldl1 (.) handleAll expr'
          where expr' = Expression <$> groupBy ((==) `on` isOp) expr
                handleAll = handle <$> possiblePrecedences
                handle prec (left : op@Operator{} : right : more) = handleOp (getPrec op)
                  where handleOp actualPrec
                          | actualPrec == prec = handle prec $ Expression [left, op, right] : more
                          | otherwise         = left : handle prec (op:right:more)
                handle prec (left:more) = left : handle prec more
                handle _ val            = val
        handleInfix val = val
        
        getPrec str = fromMaybe defaultPrecedence (lookup str precs)

        desugar (Expression [op@Operator{}, right]) = section [sId, op, right]
        desugar (Expression [left, op@Operator{}])  = section [left, op, sId]
        desugar (Expression [a, Operator op, b])  = desugar $ Expression [Id op, a, b]
        desugar (Expression expr)                   = Expression $ normalize precs <$>  expr
        desugar (ListLiteral items)                 = Expression $ normalize precs <$> items
        desugar (Operator op)                       = Id op
        desugar val                                 = val

sId :: Term                      -- The id used for operator section arguments
sId = Id "*α"

section :: [Term] -> Term
section = Lambda [sId] . Expression

isOp :: Term -> Bool
isOp Operator{} = True
isOp _          = False