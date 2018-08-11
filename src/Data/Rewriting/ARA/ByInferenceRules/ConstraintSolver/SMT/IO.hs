{-# LANGUAGE OverloadedStrings #-}
-- IO.hs ---
--
-- Filename: IO.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun May 22 19:14:44 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Aug  6 16:44:25 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 358
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--
--
--
--
--

-- Code:

module Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.IO
    ( solveSMTProblem
    ) where

import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.ConvertToSMTProblem
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.Type
import           Data.Rewriting.ARA.ByInferenceRules.Data.Type
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Exception

import           Control.Exception
                                                                                               (throw)
import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map                                                                     as M
import qualified Data.Set                                                                     as S
import qualified Data.Text                                                                    as T
import           System.IO
import qualified System.Process                                                               as Cmd
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec                                                hiding
                                                                                               (try)

import           Debug.Trace


getLogic :: StateT SMTProblem IO T.Text
getLogic = do
  l <- gets (^. logic)
  return $ "(set-logic " +++ l +++ ")\n"

-- funDefs :: T.Text
-- funDefs = "(define-fun max ((x Int) (y Int)) Int (ite (< x y) y x))\n"

intro :: Bool -> T.Text
intro shift
  | shift = "(set-logic QF_LIA)\n(define-fun max ((x Int) (y Int)) Int (ite (< x y) y x))\n"
  | otherwise = "(set-logic QF_NIA)\n(define-fun max ((x Int) (y Int)) Int (ite (< x y) y x))\n"

outro :: T.Text
outro = "(check-sat)\n"

getValues :: StateT SMTProblem IO T.Text
getValues = do
  getValsNeeded <- gets (^. getValueDirective)
  vs <- fmap S.elems (gets (^. vars))
  vs' <- fmap S.elems (gets (^. varsDeclOnly))
  if getValsNeeded
    then return $ "(get-value (" +++ T.unwords (vs ++ vs') +++ "))\n"
    else return ""

getDecls :: StateT SMTProblem IO T.Text
getDecls = do
  declFun <- gets (^. constDeclFun)
  vs <- fmap S.elems (gets (^. vars))
  vs' <- fmap S.elems (gets (^. varsDeclOnly))
  return $ T.concat (map declFun (vs ++ vs'))

fromComp :: Comparison -> T.Text
fromComp Eq  = "="
fromComp Geq = ">="

assertVarsGeq0 :: [T.Text] -> T.Text
assertVarsGeq0 vars =
  T.concat (map (\n -> "(assert (>= " +++ n +++ " 0))\n") vars)
  -- +++
  -- T.concat (map (\n -> "(assert (<= " +++ n +++ " 20))\n") vars)

asserts :: [(T.Text, Comparison, T.Text)] -> T.Text
asserts =
  T.concat . map (\(l,c,r) -> "(assert (" +++ fromComp c +++ " " +++ l +++ " "
                   +++ r +++ "))\n")

assertsStr :: [T.Text] -> T.Text
assertsStr = T.concat . map (\s -> "(assert " +++ s +++ ")\n")

ifAsserts :: [([(T.Text, T.Text)], [(T.Text,T.Text)])] -> T.Text
ifAsserts =
  T.concat . map (\(conds,xs) -> "(assert (ite " +++ toAndList conds +++ " " +++
                                 toAndList xs +++ " true))\n")

toAndList :: [(T.Text, T.Text)] -> T.Text
toAndList conds =
  if length conds > 1
  then bef +++ head condEq +++ " " +++ T.intercalate ") " (tail condEq) +++ ")"
  else head condEq
  where condEq = map (\(bl, br) -> "(= " +++ bl +++ " " +++ br +++ ")") conds
        bef = T.concat $ map (const "(and ") [1..length condEq-1]

dropDeclOnlyVarsFromVars :: StateT SMTProblem IO ()
dropDeclOnlyVarsFromVars = do
  vs' <- gets (^. varsDeclOnly)
  vs <-  gets (^. vars)
  let diff = S.difference vs vs'
  vars .= diff


solveSMTProblem :: Bool -> Bool -> FilePath -> StateT SMTProblem IO (M.Map String Int)
solveSMTProblem shift keepFiles tempDir = do

  -- ensure to drop double listed vars. We expect vars being a superset of varsDeclOnly
  dropDeclOnlyVarsFromVars

  ass <- gets (^. assertions)
  assStr <- gets (^. assertionsStr)
  ifs <- gets (^. ifs)
  log <- getLogic
  decls <- getDecls
  vs <- fmap S.elems (gets (^. vars))
  getVals <- getValues


  let txt = log +++ decls +++ assertVarsGeq0 vs +++ ifAsserts ifs +++
            asserts ass +++ assertsStr assStr +++ outro +++ getVals

  -- create the temporary files
  (pName, pHandle) <- liftIO (openTempFile tempDir "SMTP")
  (sName, sHandle) <- liftIO (openTempFile tempDir "SMTS")

  -- write to the temporary file
  liftIO (hPutStrLn pHandle (T.unpack txt))

  -- close the files
  liftIO (hClose pHandle)
  liftIO (hClose sHandle)

  -- execute binary and read in solution
  bin <- gets (^. programName)
  args <- gets (^. programOptions)
  let args' = unwords (map T.unpack args)

  -- Cmd.system
  _ <- liftIO (Cmd.system $
               T.unpack bin ++ " " ++ args' ++ " " ++ pName ++ " > " ++ sName)
  solStr <- liftIO (readFile sName)
  unless keepFiles
    (liftIO (void (Cmd.system ("rm " ++ pName  ++ " "  ++ sName ))))

  solParser <- gets (^. parseFunction)

  case parse solParser sName solStr of
    Left err -> throw $ UnsolveableException (show err)
    Right xs -> return (M.fromList xs)


--
-- IO.hs ends here
