module Core where

import Expr

data CExpr =
    CApp CExpr [CExpr]
  -- | Local reference
  | CVar Name
  -- | Global reference
  | CRef Name
  | CLit Literal
--  | CFun [Param] Type CExpr
  deriving (Show, Eq)

data CStmt =
    CDefFun Name [Param] Type CExpr
  | CDefData Name Data
  | CDefGlobal Name Type CExpr
  | CRawStmt String
  deriving (Show, Eq)
