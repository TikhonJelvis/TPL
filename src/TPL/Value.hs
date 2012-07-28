module TPL.Value where

data Term = NullLiteral
          | Id String
          | NumericLiteral Integer
          | StringLiteral String
          | BoolLiteral Bool
          | Operator String
          | ListLiteral [Term]
          | Lambda [Term] Term
          | ExpressionLiteral [Term]
          | Block [Term]
          | ObjectLiteral [(Term, Term)] deriving (Eq)

type AST = [Term]