-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Sep  5 08:52:29 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 14:34:03 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 252
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

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.Prove.Type
    ( Prove (..)
    , InfTreeNode (..)
    -- , CtxStatement (..)
    , InfTreeNodeView (..)
    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants


data Prove = Prove
    { infTreeNodesToProve :: [InfTreeNode]
    , provenInfTreeNodes  :: [InfTreeNode]
    , countCostVars       :: Int
    , problem             :: ProblemSig
    , costFreeSigs        :: CfSigs
    , signatureMap        :: ASigs
    , conditions          :: ACondition Int Int
    , varNr               :: Int
    , lhsArgDefSyms       :: [String]
    } deriving (Show)


--
-- Type.hs ends here

