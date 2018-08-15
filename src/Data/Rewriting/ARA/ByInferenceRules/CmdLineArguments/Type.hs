-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 12:19:36 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Wed Aug 15 10:18:58 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 94
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

-- Code:


module Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments.Type
    ( ArgumentOptions (..)
    , SMTSolver (..)
    ) where

import qualified Data.Text as T

data SMTSolver = Z3 | MiniSMT
  deriving (Show, Eq)

data ArgumentOptions = ArgumentOptions
    { filePath                :: FilePath
    , maxVectorLength         :: Int
    , minVectorLength         :: Int
    , uniqueConstrFuns        :: Bool
    , separateBaseCtr         :: Bool
    , helpText                :: Bool
    , tempFilePath            :: FilePath
    , keepFiles               :: Bool
    , printInfTree            :: Bool
    , verbose                 :: Bool
    , shift                   :: Bool
    , allowLowerSCC           :: Bool
    , allowCf                 :: Bool
    , lowerbound              :: Bool
    , lowerboundArg           :: Maybe Int
    , constructorArgSelection :: [(T.Text,Int)]
    , lowerboundNoComplDef    :: Bool
    , timeout                 :: Maybe Int
    , smtSolver               :: SMTSolver
    , findStrictRules         :: Maybe Int
    , directArgumentFilter    :: Bool
    , nrOfRules               :: Maybe Int    -- value not to set, only to pass through system
    } deriving (Show, Eq)


--
-- Type.hs ends here
