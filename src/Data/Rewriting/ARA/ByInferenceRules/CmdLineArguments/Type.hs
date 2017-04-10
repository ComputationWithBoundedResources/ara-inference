-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 12:19:36 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Mar  6 12:49:26 2017 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 73
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
    ) where

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
    } deriving (Show, Eq)


--
-- Type.hs ends here
