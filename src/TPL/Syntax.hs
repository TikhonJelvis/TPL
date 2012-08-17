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

normalize :: [(String, Int)] -> Term -> Term -- Take care of operator precedence and sections
normalize precs = unReifyApps . desugar . handleInfix . reifyApps
  where handleInfix (Expression expr) = squash . Expression $ foldl1 (.) handleAll expr'
          where expr' = squash . Expression <$> groupBy ((==) `on` isOp) expr
                handleAll = handle <$> possiblePrecedences
                handle prec (left : op@Operator{} : right : more) = handleOp (getPrec op)
                  where handleOp actualPrec
                          | actualPrec == prec = handle prec $ Expression [left, op, right] : more
                          | otherwise         = left : handle prec (op:right:more)
                handle prec (left:more) = left : handle prec more
                handle _ val            = val
        handleInfix val = val
        
        app = "*application*"
        reifyApps (Expression ls) = Expression $ go ls
          where go (a:b:rest) | not $ isOp b || isOp a = a : Operator app : go (b:rest)
                              | otherwise           = a : go (b:rest)
                go a                                = a
        reifyApps x               = x
        unReifyApps (Expression [op, a, b]) | op == Id app = Expression [a, b]
                                            | otherwise   = Expression [op, a, b]
        unReifyApps x                                     = x

        getPrec (Operator str) = fromMaybe defaultPrecedence $ lookup str precs
        getPrec _              = defaultPrecedence

        desugar (Expression [op@Operator{}, right]) = section [sId, op, right]
        desugar (Expression [left, op@Operator{}])  = section [left, op, sId]
        desugar (Expression [a, Operator op, b])    = desugar $ Expression [Id op, a, b]
        desugar (Expression expr)                   = Expression $ normalize precs <$>  expr
        desugar (ListLiteral items)                 = ListLiteral $ normalize precs <$> items
        desugar (Block terms)                       = Block $ normalize precs <$> terms
        desugar (Operator op)                       = Id op
        desugar val                                 = val

sId :: Term                      -- The id used for operator section arguments
sId = Id "*α"

section :: [Term] -> Term
section = Lambda [sId] . Expression

isOp :: Term -> Bool
isOp Operator{} = True
isOp _          = False

squash :: Term -> Term
squash (Expression [term]) = term
squash (Block [term])      = term
squash term                = term