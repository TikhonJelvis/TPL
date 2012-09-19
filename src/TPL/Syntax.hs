module TPL.Syntax where

import           Data.Functor ((<$>))

import           TPL.Value

-- TODO: Support left/right associative operators?
--       I currently only have left associative operators.
processOp :: String -> Term -> Term
processOp op = squash . deleteApps . handleOp . reifyApps . desugar
  where handleOp e@(Expression expr)
          | Operator op `elem` expr = Expression $ squash <$> [Id op, Expression left, Expression right]
          | otherwise               = e
          where right = reverse . takeWhile (/= Operator op) $ reverse expr
                left = reverse . drop 1 . dropWhile (/= Operator op) $ reverse expr
        handleOp notExpression = notExpression

        app = "*application*"
        notApp (Id x)       = x /= app
        notApp (Operator o) = o /= app
        notApp _            = True
        reifyApps (Expression ls) = Expression $ go ls
          where go (a:b:rest) | not $ isOp b || isOp a = a : Operator app : go (b : rest)
                              | otherwise           = a : go (b : rest)
                go a                                = a
        reifyApps x                  = x
        deleteApps (Expression expr) = Expression $ deleteApps <$> filter notApp expr
        deleteApps x                 = x

isOp :: Term -> Bool
isOp Operator{} = True
isOp _          = False

squash :: Term -> Term
squash (Expression [term]) = term
squash (Block [term])      = term
squash term                = term


desugar :: Term -> Term
desugar (Expression [Operator o, right])  = Expression [Id "flip", Id o, right]
desugar (Expression [left, Operator o])   = Expression [Id o, left]
desugar (Expression [Operator o])         = Id o
desugar val                               = val
