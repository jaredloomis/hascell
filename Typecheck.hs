module Typecheck where

import qualified Data.Map as M

import Expr

data TypeError =
    UnboundVar Name
  | NotAFunction Expr
  | TypeMismatch Type Type
  deriving (Show, Eq)

typeOf :: Ctx -> Expr -> Either TypeError Type 
typeOf ctx (App f x) = do
    tyF <- typeOf ctx f
    (tyX, tyB) <- case tyF of
        ArrowTy a b -> return (a, b)
        _           -> Left (NotAFunction f)
    tyX' <- typeOf ctx x

    if tyX == tyX'
        then return tyB
        else Left $ TypeMismatch tyX tyX'
typeOf ctx (Lam n tyN b) =
    let ctx' = M.insert n tyN ctx
    in ArrowTy tyN <$> typeOf ctx' b
typeOf ctx (Lit l) = typeLit ctx l
typeOf ctx (Var n) =
    maybe (Left $ UnboundVar n) return (M.lookup n ctx)

typeLit :: Ctx -> Literal -> Either TypeError Type
typeLit _ LitInt{}    = return $ ScalarTy IntTy
typeLit _ LitFlt{}    = return $ ScalarTy FloatTy
typeLit _ LitStr{}    = return $ PtrTy (ScalarTy CharTy)
typeLit c (LitPrim p) = typePrim c p

typePrim :: Ctx -> Prim -> Either TypeError Type
typePrim _ (PrimVar ty _) = return ty
typePrim _ (PrimBinOp a b c _) = return .
    ArrowTy a . ArrowTy b $ c
typePrim _ (PrimUnOp a b _ _) = return $
    ArrowTy a b
typePrim _ PrimSequenceOp =
    return $ ArrowTy (IOTy UnknownTy) $
             ArrowTy (IOTy UnknownTy) (IOTy UnknownTy)
