-- ParseSolutions.hs ---
--
-- Filename: ParseSolutions.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Apr 14 13:24:29 2017 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 20
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

module Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.ParseSolutions
  ( parseZ3
  , parseMinismt
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

parseMinismt :: Parser [(String, Int)]
parseMinismt = unkown <|> try unsolveable <|> solutionMinismt <|> timeoutMinismt <|> parseError

parseZ3 :: Parser [(String, Int)]
parseZ3 = unkown <|> unsolveable <|> solutionZ3 <|> timeout <|> parseError

parseError :: Parser a
parseError = throw $ ParseException "Could not parse solution. Programming error."

unkown :: Parser a
unkown = do
  _ <- try (string "unknown")
  parserFail "The smt solver returned 'unkown'."

unsolveable :: Parser a
unsolveable = do
  _ <- string "unsat"
  fail "The smt problem could not be solved (was unsat)."

timeout :: Parser a
timeout = do
  _ <- string "timeout"
  fail "The smt solver ran in a timeout."

timeoutMinismt :: Parser a
timeoutMinismt = do
  _ <- string "unusual termination"
  fail "The smt solver ran in a timeout."

solutionMinismt :: Parser [(String, Int)]
solutionMinismt = do
  _ <- string "sat" >> spaces
  _ <- many digit >> char '.' >> many digit >> spaces
  v <- many varAssignment
  _ <- eof
  return v

solutionZ3 :: Parser [(String, Int)]
solutionZ3 = do
  _ <- string "sat" >> spaces
  _ <- char '('
  v <- many varMap
  _ <- char ')' >> spaces
  _ <- eof
  return v

varAssignment :: Parser (String, Int)
varAssignment = do
  _ <- spaces
  n <- name
  _ <- spaces >> string "=" >> spaces
  v <- int
  _ <- spaces
  return (n,v)


varMap :: Parser (String, Int)
varMap = do
  _ <- spaces >> char '('
  n <- name
  _ <- spaces
  v <- int
  _ <- char ')' >> spaces
  return (n, v)

name :: Parser String
name = do
  l <- letter
  r <- many (alphaNum <|> char '_' <|> char '\'')
  return $ l:r


int :: Parser Int
int = do
  ds <- (do _ <- char '('
            _ <- char '-' >> spaces
            nr <- many digit
            _ <- char ')'
            return ('-':nr))
        <|>
        (do  _<- char '-'
             nr <- many digit
             return ('-':nr))
        <|>
        many digit
  return (read ds :: Int)


--
-- ParseSolutions.hs ends here
