-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Sep  5 08:52:29 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat May  6 19:37:26 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 253
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


data Prove f v s sDt dt c = Prove
    { infTreeNodesToProve :: [InfTreeNode]
    , provenInfTreeNodes  :: [InfTreeNode]
    , countCostVars       :: Int
    , problem             :: ProblemSig f v s sDt dt c
    , costFreeSigs        :: CfSigs
    , signatureMap        :: ASigs
    , conditions          :: ACondition Int Int
    , varNr               :: Int
    , lhsArgDefSyms       :: [String]
    } deriving (Show)


--
-- Type.hs ends here

