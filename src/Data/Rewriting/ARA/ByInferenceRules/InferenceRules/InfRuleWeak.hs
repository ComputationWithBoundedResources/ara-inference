{-# LANGUAGE ScopedTypeVariables #-}
-- InfRuleWeak.hs ---
--
-- Filename: InfRuleWeak.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 11:39:45 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Jun 18 20:02:40 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 160
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
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleWeak
    ( weak

    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleFunction
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.Operator.Type
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.Typed.Term                                          hiding
                                                                                     (map)
import qualified Data.Rewriting.Typed.Term                                          as T

import           Control.Arrow
import           Data.List
                                                                                     (nub)

import           Debug.Trace
                                                                                     (trace)

weak :: forall f v dt . (Eq v, Eq dt, Read v, Ord v, Show v, Show dt, Show f) =>
        ArgumentOptions
     -> (ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
          ACondition f v Int Int, InfTreeNode f v dt)
     -> [(ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
           ACondition f v Int Int, [InfTreeNode f v dt])]
weak args (prob, cfsigs, asigs, nr, conds, InfTreeNode pre cst (Just (term, dt))
            i@(fn,ruleStr,isCtrDeriv,startCsts,sigNr,mCfSigIdx) his) =
  -- trace ("preweak: " ++ show (pre,term))
  -- trace ("weak: " ++ show (length pre > length varsTerm))
  [(prob, cfsigs, asigs, nr, conds', [InfTreeNode pre' cst (Just (term, dt)) i his'])
  | length pre > length varsTerm -- only if more variables left than right
  ]

  where pre' = filter (\x -> fst x `elem` varsTerm) pre
        filteredPre = filter (\x -> fst x `notElem` varsTerm) pre
        nDtConds = map (\x -> (removeDt (snd x), Eq, 0)) filteredPre
        conds'
          | isCtrDeriv = conds
          | otherwise = conds { dtConditionsInt = dtConditionsInt conds ++ nDtConds }
        varsTerm = vars term
        his' = his ++ [(fst3 (last his) + 1, "weak",
                        InfTreeNodeView (map (show *** toADatatypeVectorString) pre')
                        (map toACostConditionVector cst)
                        (T.map show show term, toADatatypeVectorString dt))]

weak _ _ = []

--
-- InfRuleWeak.hs ends here
