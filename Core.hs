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
  | CDefData CData
  | CDefGlobal Name Type CExpr
  | CRawStmt String
  deriving (Show, Eq)

data CData =
    CStruct Name [(Name, Type)]
  | CUnion  Name [Type]
--  | CUnion  Name [CData]
  deriving (Show, Eq)

{-

data X = MkX (xi :: Int) | MkY (yi :: Int)

||||||
||||||
vvvvvv

struct MkX {
    int xi;
};

struct MkY {
    int yi;
};

enum X {
    struct MkX;
    struct MkY;
};

-}
