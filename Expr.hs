module Expr where

import qualified Data.Map as M

type Name  = String
type Param = (String, Type)

type Env   = M.Map Name Expr
type Ctx   = M.Map Name Type

-- Type

data Type =
    ScalarTy ScalarType
  | PtrTy Type
  | ArrayTy Type
  | DataTy Name
  | ArrowTy Type Type
  | IOTy Type
  | UnknownTy
--  | AnyTy
  deriving (Show, Eq)

data ScalarType =
    VoidTy
  | IntTy   | ShortTy   | CharTy
  | FloatTy | DoubleTy
  | Unsigned ScalarType | Long ScalarType
  deriving (Show, Eq)

-- Expr

data Expr =
    App Expr Expr
  | Lam Name Type Expr
  | Var Name
  | Lit Literal
  deriving (Show, Eq)

data Literal =
    LitInt Integer
  | LitFlt Double
  | LitStr String
  | LitPrim Prim
  deriving (Show, Eq)

data Prim =
    PrimVar   Type Name
  | PrimBinOp Type Type Type Name
  | PrimUnOp  Type Type UnaryFix Name
  | PrimSequenceOp
  deriving (Show, Eq)

data UnaryFix = Prefix | Postfix
  deriving (Show, Eq)

-- Stmt

data Stmt =
    DefExpr Name Expr (Maybe Type)
  | DefData Name Data
  | DefForeign Name Type
  | RawStmt String
  deriving (Show, Eq)

data Data =
    -- 'Pair' type
    Product Name [Type]
    -- 'Either' type
  | Sum [Data]
  deriving (Show, Eq)
