-- InfRuleIdentity.hs ---
--
-- Filename: InfRuleIdentity.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 03:42:33 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 18:20:55 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 178
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

{-# LANGUAGE CPP #-}

-- #define DEBUG

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleIdentity
    ( identity

    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.Typed.Problem

#ifdef DEBUG
import           Debug.Trace                                                    (trace)
#endif

-- | The identity inference rule.
identity :: (ProblemSig, CfSigs, ASigs, Int, ACondition Int Int, InfTreeNode)
         -> [(ProblemSig, CfSigs, ASigs, Int, ACondition Int Int, [InfTreeNode])]
identity (prob, cfsigs, asigs, nr, conds, InfTreeNode [pre] cst (Just post)
           i@(_,_,isCtrDeriv,_,_,_) his) =

  [(prob, cfsigs, asigs, nr, nConds
   , [ InfTreeNode [] [] Nothing i (his ++ [(fst3 (last his) + 1, "identity",
                                              InfTreeNodeLeafEmpty)])])
  | fst pre == funName ]

    where condDt :: [([ADatatype Int], Comparison, [ADatatype Int])]
          condDt = [([snd pre], if isCtrDeriv then Eq else Geq, [snd post])]
          condCst :: [([ACostCondition Int], Comparison, [ACostCondition Int])]
          condCst = [(cst, if isCtrDeriv then Eq else Geq, [ACostValue 0])]
          nConds = ACondition (costCondition conds ++ condCst) (dtConditions conds ++ condDt)
                      (shareConditions conds)
          funName = termName (fst post)
identity _ = []


--
-- InfRuleIdentity.hs ends here
