-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 12:19:36 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Fri Jun 16 15:24:30 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 85
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

data SMTSolver = Z3 | MiniSMT
  deriving (Show, Eq)

data ArgumentOptions = ArgumentOptions
    { filePath         :: FilePath
    , maxVectorLength  :: Int
    , minVectorLength  :: Int
    , uniqueConstrFuns :: Bool
    , separateBaseCtr  :: Bool
    , helpText         :: Bool
    , tempFilePath     :: FilePath
    , keepFiles        :: Bool
    , printInfTree     :: Bool
    , verbose          :: Bool
    , shift            :: Bool
    , allowLowerSCC    :: Bool
    , lowerbound       :: Bool
    , lowerboundArg    :: Maybe Int
    , timeout          :: Maybe Int
    , smtSolver        :: SMTSolver
    , findStrictRules  :: Maybe Int
    -- , disableCf        :: Bool
    } deriving (Show, Eq)


--
-- Type.hs ends here
