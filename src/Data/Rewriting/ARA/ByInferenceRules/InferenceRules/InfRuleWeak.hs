-- InfRuleWeak.hs ---
--
-- Filename: InfRuleWeak.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 11:39:45 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 14:34:05 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 109
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
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleWeak
    ( weak

    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleFunction
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.Typed.Term                                          hiding
                                                                                     (map)


import           Control.Arrow

#ifdef DEBUG
import           Debug.Trace
                                                                                     (trace)
#endif

weak :: (ProblemSig, CfSigs, ASigs, Int, ACondition Int Int, InfTreeNode)
         -> [(ProblemSig, CfSigs, ASigs, Int, ACondition Int Int, [InfTreeNode])]
weak (prob, cfsigs, asigs, nr, conds, InfTreeNode pre cst (Just (term, dt)) fn his) =
  -- trace ("weak")

  [(prob, cfsigs, asigs, nr, conds, [InfTreeNode pre' cst (Just (term, dt)) fn his'])
  | length pre > length varsTerm -- only if more variables left than right
  ]

  where pre' = filter (\x -> fst x `elem` varsTerm) pre
        varsTerm = vars term
        his' = his ++ [(fst3 (last his) + 1, "weak",
                        InfTreeNodeView (map (second toADatatypeVector) pre')
                        (map toACostConditionVector cst) (term, toADatatypeVector dt))]

weak _ = []

--
-- InfRuleWeak.hs ends here
