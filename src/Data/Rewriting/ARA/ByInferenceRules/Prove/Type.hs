-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Sep  5 08:52:29 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 22:31:47 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 260
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


data Prove f v s sDt dt cn = Prove
    { infTreeNodesToProve :: [InfTreeNode f v dt]
    , provenInfTreeNodes  :: [InfTreeNode f v dt]
    , countCostVars       :: Int
    , problem             :: ProblemSig f v s sDt dt cn
    , costFreeSigs        :: CfSigs dt s
    , signatureMap        :: ASigs dt s
    , conditions          :: ACondition f v Int Int
    , varNr               :: Int
    , lhsArgDefSyms       :: [f]
    } deriving (Show)


--
-- Type.hs ends here

