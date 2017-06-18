{-# LANGUAGE ScopedTypeVariables #-}
-- InfRuleIdentity.hs ---
--
-- Filename: InfRuleIdentity.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 03:42:33 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Jun 18 20:33:55 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 198
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

{-# LANGUAGE CPP                 #-}

-- #define DEBUG

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleIdentity
    ( identity

    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.Typed.Problem

import           Data.Maybe                                                     (isJust)

import           Debug.Trace                                                    (trace)

-- | The identity inference rule.
identity :: forall f v dt . (Show f, Show v) =>
            ArgumentOptions
         -> (ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
             ACondition f v Int Int, InfTreeNode f v dt)
         -> [(ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
              ACondition f v Int Int, [InfTreeNode f v dt])]
identity args (prob, cfsigs, asigs, nr, conds, InfTreeNode [pre] cst (Just post)
           i@(_,_,isCtrDeriv,_,_,_) his) =

  -- (if paramNr (snd pre) == 9
  -- then trace ("term: " ++ show (pre, post))
  --      trace ("nConds: " ++ show condDt)
  --      trace ("show (fst pre) == funName: " ++ show (show (fst pre) == funName))
  -- else id)
  [(prob, cfsigs, asigs, nr, nConds
   , [ InfTreeNode [] [] Nothing i (his ++ [(fst3 (last his) + 1, "identity",
                                              InfTreeNodeLeafEmpty)])])
  | show (fst pre) == funName ]

    where condDt = [([removeDt $ snd pre], if isCtrDeriv then Eq else geq, [removeDt $ snd post])]
          condCst = [(cst, if isCtrDeriv then Eq else geq, [ACostValue 0])]
          nConds = ACondition (costCondition conds ++ condCst) (dtConditions conds ++ condDt)
                   (dtConditionsInt conds) (shareConditions conds) (minus1Vars conds)
          funName = termName (fst post)
          geq | isJust (lowerboundArg args) || lowerbound args = Leq
              | otherwise = Geq
          paramNr (SigRefParam _ nr _) = nr
          paramNr _                    = -1
identity _ _ = []


--
-- InfRuleIdentity.hs ends here
