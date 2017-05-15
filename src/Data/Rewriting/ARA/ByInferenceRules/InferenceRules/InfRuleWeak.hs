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
-- Last-Updated: Mon May  8 09:35:37 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 110
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
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleFunction
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.Typed.Term                                          hiding
                                                                                     (map)
import qualified Data.Rewriting.Typed.Term                                          as T


import           Control.Arrow

#ifdef DEBUG
import           Debug.Trace
                                                                                     (trace)
#endif

weak :: forall f v dt . (Eq v, Eq dt, Read v, Ord v, Show v, Show dt, Show f) =>
         (ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
           ACondition f v Int Int, InfTreeNode f v dt)
      -> [(ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
            ACondition f v Int Int, [InfTreeNode f v dt])]
weak (prob, cfsigs, asigs, nr, conds, InfTreeNode pre cst (Just (term, dt)) fn his) =
  -- trace ("weak")

  [(prob, cfsigs, asigs, nr, conds, [InfTreeNode pre' cst (Just (term, dt)) fn his'])
  | length pre > length varsTerm -- only if more variables left than right
  ]

  where pre' = filter (\x -> fst x `elem` varsTerm) pre
        varsTerm = vars term
        his' = his ++ [(fst3 (last his) + 1, "weak",
                        InfTreeNodeView (map (show *** toADatatypeVectorString) pre')
                        (map toACostConditionVector cst)
                        (T.map show show term, toADatatypeVectorString dt))]

weak _ = []

--
-- InfRuleWeak.hs ends here
