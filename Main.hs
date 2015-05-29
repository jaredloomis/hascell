module Main where

import Data.List (isPrefixOf, isSuffixOf)
import System.Environment (getArgs)
import Control.Monad.State

import Codegen
import Parser
import Elab

main :: IO ()
main = do
    args <- getArgs
    let ins = inFiles args
        out = outFile args
    writeC out (head ins)

writeC :: FilePath -> FilePath -> IO ()
writeC outf inf = do
    src <- readFile inf
    case genC inf src of
        Left  err  -> putStrLn $ "ERROR:\n" ++ err
        Right code -> do
            putStrLn "Successfully compiled"
            writeFile outf code

genC :: FilePath -> String -> Either String String
genC f src =
    case iparse (allOfMany parseStmt) f src of
        Left  err   -> Left $ show err
        Right stmts -> execElab $ elabFull mempty stmts
--intercalate "\n" <$> mapM stmtToC' stmts
--foldlM (fmap (++) . stmtToC') "" stmts

execElab :: ElabM a -> Either String String
execElab elab =
    case execStateT elab (ElabState mempty [] 0) of
        Right st -> return $ concatMap toC (elabStmts st)
        Left err -> Left   $ show err


inFiles :: [String] -> [FilePath]
inFiles = filter (isSuffixOf ".hsc")

outFile :: [String] -> FilePath
outFile args =
    case dropWhile (not . isPrefixOf "-o") args of
        ("-o" : file : _) -> file
        _                 -> "out.c"
