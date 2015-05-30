module Elab where

import Data.List (find)
import Data.Monoid hiding (Product, Sum)
import Control.Monad.State
import Control.Arrow
import Data.Foldable (foldlM)

import qualified Data.Map as M

import Expr
import Typecheck
import Optimize
import Core

type ElabM = StateT ElabState (Either TypeError)

data ElabState = ElabState {
    -- Global context
    elabCtx   :: Ctx,
    elabStmts :: [CStmt],
    nameSeed :: Int
    } deriving (Show, Eq)

elabFull :: Ctx -> [Stmt] -> ElabM ()
elabFull ctx xs = do
    collectCtx ctx xs
    mapM_ (elabUnify ctx >=> elabStmt ctx) xs

elabUnify :: Ctx -> Stmt -> ElabM Stmt
elabUnify ctx stmt@(DefExpr n e (Just ty)) = do
    ctx' <- (ctx <>) <$> gets elabCtx
    return $ either
           (const stmt)
           (flip (DefExpr n) (Just ty))
           (unify ctx' e ty)
elabUnify _ stmt = return stmt

collectCtx :: Ctx -> [Stmt] -> ElabM ()
collectCtx ctx xs = mapM_ regStmt xs
  where
    regStmt :: Stmt -> ElabM ()
    regStmt (DefExpr n _ (Just ty)) = addGlobal n ty
    regStmt (DefExpr n e Nothing) = do
        ctx' <- (ctx <>) <$> gets elabCtx
        tyE <- typeOf' ctx' e
        addGlobal n tyE
    regStmt (DefForeign n ty) =
        addGlobal n ty
    regStmt (DefData n _) =
        addGlobal n TypeTy
    regStmt _ = return ()

    typeOf' :: Ctx -> Expr -> ElabM Type
    typeOf' ctx' e =
        case typeOf ctx e of
            Left (UnboundVar n) ->
                case lookupStmt n of
                    Just stmt -> do
                        regStmt stmt
                        ctx'' <- (ctx' <>) <$> gets elabCtx
                        lift $ typeOf ctx'' e
                    Nothing -> lift $ Left (UnboundVar n)
            err@Left{} -> lift err
            Right ty -> return ty

    lookupStmt :: Name -> Maybe Stmt
    lookupStmt n = find checkName xs
      where
        checkName (DefExpr    n' _ _) | n == n' = True
        checkName (DefForeign n' _)   | n == n' = True
        checkName _                             = False

elabStmt :: Ctx -> Stmt -> ElabM ()
elabStmt ctx (DefExpr n e _)  = defFun ctx n e
elabStmt _   (DefForeign _ _) = return ()
elabStmt _   (DefData    n d) = elabData n d
elabStmt _   (RawStmt    s)   = addStmt $ CRawStmt s

elabData :: Name -> Data -> ElabM ()
elabData _ (Product n fs) = --return $ CStruct n fs
    addStmt . CDefData $ CStruct n fs
elabData n (Sum xs)       = do -- CUnion n <$> mapM (elabData $ n ++ "_0") xs
    -- Elab fields
    _ <- foldlM elab' 0 xs

    -- Represent fields as 'struct n;'
    addStmt . CDefData . CUnion n $ map asType xs
  where
    asType :: Data -> Type
    asType (Product pn _) = StructTy pn
    asType (Sum _) = error "elabData.fieldName recieved Sum"

    elab' :: Int -> Data -> ElabM Int
    elab' i x = elabData (n ++ "_" ++ show i) x >> return (i+1)

elabExpr :: Ctx -> Expr -> ElabM CExpr
elabExpr ctx e@Lam{} = do
    name <- newName
    defFun ctx name e
    return $ CRef name
elabExpr ctx (Var n) = do
    globalCtx <- gets elabCtx
    maybe (maybe (lift  . Left   $ UnboundVar n)
                 (const . return $ CRef n)
                 (M.lookup n globalCtx))
          (const . return $ CVar n)
          (M.lookup n ctx)
elabExpr ctx (App f x) = do
    let (f', args) = second (++ [x]) $ gatherArgs f
    CApp <$> elabExpr ctx f'
         <*> mapM (elabExpr ctx) args
elabExpr _ (Lit l) = return $ CLit l

defFun :: Ctx -> Name -> Expr -> ElabM ()
defFun ctx name e = do
    globalCtx <- gets elabCtx
    let (params, body) = gatherParams e
        localCtx = ctx <> M.fromList params
        totalCtx = ctx <> M.fromList params <> globalCtx
    retTy <- lift $ typeOf totalCtx body

    addStmt . CDefFun name params retTy =<< elabExpr localCtx body

gatherParams :: Expr -> ([Param], Expr)
gatherParams (Lam n tyN b) = first ((n, tyN) :) (gatherParams b)
gatherParams e             = ([], e)

gatherArgs :: Expr -> (Expr, [Expr])
gatherArgs (App f x) = second (++ [x]) (gatherArgs f)
gatherArgs f         = (f, [])

terminalType :: Type -> Type
terminalType (ArrowTy _ b) = terminalType b
terminalType b             = b

newName :: ElabM Name
newName = do
    es <- get
    let seed = nameSeed es
    put es{nameSeed = seed+1}
    return $ nameFromSeed seed

nameFromSeed :: Int -> String
nameFromSeed = ("hascell_ident_" ++) . show

addStmt :: CStmt -> ElabM ()
addStmt stmt = modify $ \es ->
    es{elabStmts = elabStmts es ++ [stmt]}

addGlobal :: Name -> Type -> ElabM ()
addGlobal n ty = modify $ \es ->
    es{elabCtx = M.insert n ty (elabCtx es)}
