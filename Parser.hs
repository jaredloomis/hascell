--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Parser where

import Data.List (foldl1')
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State

import Text.Parsec hiding (State)
import Text.Parsec.Pos (initialPos)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

import Expr
import Codegen

type IParser a = ParsecT String () (State SourcePos) a

iparse :: IParser a -> SourceName -> String -> Either ParseError a
iparse p n s = runIdentity .
    flip evalStateT (initialPos n) $ runParserT p () n s

-----------------
-- Stmt Parser --
-----------------

parseStmt :: IParser Stmt
parseStmt = try foreignStmt <|> typedVal

foreignStmt :: IParser Stmt
foreignStmt = do
    lexeme (reserved "foreign")
    uncurry DefForeign <$> lexeme typedIdent

typedVal :: IParser Stmt
typedVal = do
    (name, ty) <- lexeme typedIdent

    _          <- lexeme (string name)
    params <- lexeme parseParams
    lexeme (reservedOp "=")

    flip (DefExpr name) (Just ty) . addParams params <$>
        lexeme parseExpr

addParams :: [Param] -> Expr -> Expr
addParams ((n, ty):xs) b = addParams xs $ Lam n ty b
addParams _            b = b

parseParams :: IParser [Param]
parseParams = map (,UnknownTy) <$> many (lexeme identifier)

-----------------
-- Expr Parser --
-----------------

parseExpr :: IParser Expr
parseExpr = try lambda <|> try primVar <|> try app <|> atom

lambda :: IParser Expr
lambda = do
    reservedOp "\\"
    name <- same . lexeme $ identifier
    sameOrIndented . lexeme $ reservedOp "->"

    body <- sameOrIndented parseExpr
    return $ Lam name UnknownTy body

app :: IParser Expr
app = foldl1' App <$> many1 (sameOrIndented $ lexeme atom)

atom :: IParser Expr
atom = try literal <|> try var <|> parens parseExpr

var :: IParser Expr
var = Var <$> identifier

literal :: IParser Expr
literal = Lit <$> lit

lit :: IParser Literal
lit =   try (LitStr <$> Tok.stringLiteral tokenParser)
    <|> try (LitFlt <$> Tok.float         tokenParser)
    <|>     (LitInt <$> Tok.integer       tokenParser)
    -- TODO COMPLETE

typedIdent :: IParser (Name, Type)
typedIdent =
    (,) <$> lexeme identifier <*  sameOrIndented (lexeme (reservedOp "::"))
                              <*> sameOrIndented (lexeme typeP)

-----------------
-- Type Parser --
-----------------

typeP :: IParser Type
typeP = try arrowTy <|> ioTy <|> atomTy

atomTy :: IParser Type
atomTy = try (ScalarTy <$> scalarTy) <|> parens typeP
    -- TODO COMPLETE

arrowTy :: IParser Type
arrowTy = do
    a <- lexeme atomTy
    lexeme $ reservedOp "->"
    b <- lexeme typeP
    return $ ArrowTy a b

ioTy :: IParser Type
ioTy = do
    lexeme $ reserved "IO"
    IOTy <$> typeP

scalarTy :: IParser ScalarType
scalarTy =
    try (IntTy <$ string "Int")
--    <|> (Cha)
    -- TODO COMPLETE

primVar :: IParser Expr
primVar = do
    lexeme (string "primVar")
    Lit . LitPrim <$> (flip PrimVar
        <$> lexeme (Tok.stringLiteral tokenParser)
        <*> typeP)

------------------
-- Token Parser --
------------------

tokenParser :: Tok.GenTokenParser String a (State SourcePos)
tokenParser =
    let tp = emptyDef {
        Tok.reservedOpNames = ["=", "\\", "->", "::"],
        Tok.reservedNames   = ["foreign", "IO"],

        Tok.commentStart    = "{-",
        Tok.commentEnd      = "-}",
        Tok.commentLine     = "--",
        Tok.nestedComments  = True,
        Tok.identStart      = letter,
        Tok.identLetter     = alphaNum <|> oneOf "_'",
        Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter        = Tok.opStart tp,
        Tok.caseSensitive   = True
        }
    in Tok.makeTokenParser tp

reservedOp :: String -> IParser ()
reservedOp = Tok.reservedOp tokenParser

reserved :: String -> IParser ()
reserved = Tok.reserved tokenParser

identifier :: IParser Name
identifier = Tok.identifier tokenParser

lexeme :: IParser a -> IParser a
lexeme = Tok.lexeme tokenParser

parens :: IParser a -> IParser a
parens = Tok.parens tokenParser

operator :: IParser String
operator = Tok.operator tokenParser

----------------------
-- Operator parsing --
----------------------

{-
operators :: OperatorTable String a (State SourcePos) Expr
operators = [
    
    ]

exprBinOp :: String -> IParser (Expr -> Expr -> Expr)
exprBinOp op = do
    parseExpr'
-}

--------------------
-- Parser Helpers --
--------------------

sameOrIndented :: IParser a -> IParser a
sameOrIndented = (sameOrIndentCheck >>)

sameOrIndentCheck :: IParser ()
sameOrIndentCheck = checkSame <|> indent

indented :: IParser a -> IParser a
indented = (indent >>)

indent :: IParser ()
indent = do
    pos <- getPosition
    s <- get
    if sourceColumn pos <= sourceColumn s
        then parserFail "not indented"
    else put $ setSourceLine s (sourceLine pos)

same :: IParser a -> IParser a
same = (checkSame >>)

checkSame :: IParser ()
checkSame = do
    pos <- getPosition
    s <- get
    when (sourceLine pos /= sourceLine s) $
        parserFail "over one line"

block :: IParser a -> IParser [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r

withPos :: IParser a -> IParser a
withPos x = do
    a <- get
    p <- getPosition
    r <- put p >> x
    put a >> return r

checkIndent :: IParser ()
checkIndent = do
    s <- get
    p <- getPosition
    when (sourceColumn p /= sourceColumn s) $
        parserFail "indentation doesn't match"

allOfMany :: IParser a -> IParser [a]
allOfMany = allOf . many

allOf :: IParser a -> IParser a
allOf p = Tok.whiteSpace tokenParser *> p <* eof

infixr 5 <++>
(<++>) :: Monoid a => IParser a -> IParser a -> IParser a
(<++>) = liftA2 mappend

-------------
-- TESTING --
-------------

parsef :: IParser a -> SourceName -> String -> a
parsef p t s =
    let Right x = iparse p t s
    in x

testp = parsef parseStmt "Whatever" $
    "cnst = (\\(x :: Int) -> \\(y :: Int) -> x) 12 13"

testc = putStr $ stmtToC testp
