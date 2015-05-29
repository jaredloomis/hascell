module Optimize where

import qualified Data.Map as M

import Expr
import Typecheck

normalize :: Expr -> Expr
normalize (Lam n tyN b) =
    -- Eta reduction
    case normalize b of
        App f (Var n') | n == n' && not (n `freeIn` f) ->
            f
        b' -> Lam n tyN b'
normalize (App f x) =
    -- Beta reduction
    case normalize f of
        Lam n _ b -> normalize (subst n (normalize x) b)
        f'        -> App f' (normalize x)
normalize l@Lit{} = l
normalize v@Var{} = v

freeIn :: Name -> Expr -> Bool
freeIn n (Lam n' _ b) = n == n'    || freeIn n b
freeIn n (App f x)    = freeIn n f || freeIn n x
freeIn n (Var n')     = n == n'
freeIn _ (Lit _)      = False

whnf :: Expr -> Expr
whnf (App f x) =
    case whnf f of
        Lam n _ e -> whnf (subst n x e)
        e         -> e
whnf e = e

subst :: Name -> Expr -> Expr -> Expr
subst n e (Lam n' tyN b) = Lam n' tyN (subst n e b)
subst n e (App f x) = App (subst n e f) (subst n e x)
subst n e b@(Var n')
    | n == n'   = e
    | otherwise = b
subst _ _ l@(Lit _) = l

unify :: Ctx -> Expr -> Type -> Either TypeError Expr
unify ctx (Lam n _ e) (ArrowTy tyN tyE) =
    Lam n tyN <$> unify (M.insert n tyN ctx) e tyE
unify _ l@Lam{} _ = return l
unify ctx (App f x) tyFx = do
    tyX <- typeOf ctx x
    App <$> unify ctx f (ArrowTy tyX tyFx)
        <*> unify ctx x tyX
unify _ x _ = return x -- TODO CONTINUE
