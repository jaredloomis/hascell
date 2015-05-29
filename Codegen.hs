{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Codegen where

import Data.List (intercalate)
import Control.Monad.State

import Expr
import Core
import Elab

class ToC a where
    toC :: a -> String

instance ToC CStmt where
    toC (CDefFun name params retTy body) =
        toC retTy ++ " " ++ name ++
        "(" ++ intercalate ", " (map toC params) ++ ") {\n" ++
        "return " ++ toC body ++ ";" ++
        "\n}\n"
    toC (CDefGlobal name ty val) =
        toC ty ++ " " ++
        name ++ " = " ++ toC val ++
        ";\n"
    toC (CDefData   _ _) = error "toC CDefData   not implemented"
    toC (CRawStmt s)     = s

instance ToC Param where
    toC (n, ty) = toC ty ++ " " ++ n

instance ToC CExpr where
    toC (CApp (CRef f) xs) =
        f ++ "(" ++ intercalate ", " (map toC xs) ++ ")"
    toC (CApp (CLit (LitPrim (PrimBinOp _ _ _ f))) [x, y]) =
        toC x ++ " " ++ f ++ " " ++ toC y
    toC (CApp (CLit (LitPrim (PrimUnOp _ _ Prefix f))) [x]) =
        f ++ toC x
    toC (CApp (CLit (LitPrim (PrimUnOp _ _ Postfix f))) [x]) =
        toC x ++ f
    toC (CApp (CLit (LitPrim PrimSequenceOp)) [x, y]) =
        toC x ++ ";\n" ++ toC y
    toC (CApp f xs) =
        toC f ++ "(" ++ intercalate ", " (map toC xs) ++ ")"
    toC (CVar n) = n
    toC (CRef n) = n ++ "()"
    toC (CLit l) = toC l

instance ToC Literal where
    toC (LitInt  i) = show i
    toC (LitFlt  f) = show f
    toC (LitStr  s) = show s
    toC (LitPrim p) = toC p

instance ToC Prim where
    toC (PrimVar   _     n) = n
    toC (PrimBinOp _ _ _ n) = n
    toC (PrimUnOp  _ _ _ n) = n
    toC PrimSequenceOp      = ""

instance ToC Type where
    toC (ScalarTy s)  = toC s
    toC (PtrTy ty)    = toC ty ++ "*"
    toC (ArrayTy ty)  = toC ty ++ "[]"
    toC (DataTy n)    = n
    toC (ArrowTy _ _) = "void*"
    toC (IOTy a)      = toC a
    toC UnknownTy     = "void*"

instance ToC ScalarType where
    toC VoidTy       = "void"
    toC IntTy        = "int"
    toC ShortTy      = "short"
    toC CharTy       = "char"
    toC FloatTy      = "float"
    toC DoubleTy     = "double"
    toC (Unsigned t) = "unsigned " ++ toC t
    toC (Long t)     = "long " ++ toC t

-- TESTING --

constExpr :: Expr
constExpr =
    Lam "x" (ScalarTy IntTy) $
    Lam "y" (ScalarTy IntTy) $
    Var "x"

appExpr :: Expr
appExpr = App (App constExpr (Lit $ LitInt 12)) (Lit $ LitInt 3)

testToC :: Expr -> String
testToC e =
    let Right (ce, es) = runStateT (elabExpr mempty e)
                                   (ElabState mempty [] 0)
        cs = elabStmts es
    in "// Stmts:\n" ++ concatMap toC cs ++
       "\n// Expr:\n" ++ toC ce ++ "\n"

stmtToC :: Stmt -> String
stmtToC e =
    let Right ((), es) = runStateT (elabStmt mempty e)
                                   (ElabState mempty [] 0)
        cs = elabStmts es
    in concatMap toC cs
