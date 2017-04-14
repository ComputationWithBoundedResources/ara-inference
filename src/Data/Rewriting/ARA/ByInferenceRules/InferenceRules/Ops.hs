-- InferenceRules.hs ---
--
-- Filename: InferenceRules.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Sep 14 17:30:38 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 14:34:04 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 515
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
-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.Ops
    ( applyInferenceRules
    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants

-- Inference rules
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleComposition
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleFunction
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleIdentity
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleShare
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleWeak
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type


import           Control.Arrow
import           Debug.Trace
                                                                                        (trace)

import           Data.Maybe
                                                                                        (fromJust)
import           Text.PrettyPrint

applyInferenceRules :: ArgumentOptions -> [(String,Integer)]  -> Prove -> Either Int [Prove]
applyInferenceRules _ _ (Prove [] p c t cfs sigs cond v noCfDefSyms)        =
  return [Prove [] p c t cfs sigs cond v noCfDefSyms]
applyInferenceRules args reachability (Prove (c:cs) p count prob cfs sigs cond v noCfDefSyms) =
    case c of
         InfTreeNode [] _ Nothing _ _ ->
            applyInferenceRules args reachability (Prove cs (c:p) count prob cfs sigs cond v noCfDefSyms)
         InfTreeNode pre cost stmt fn [] ->  -- use share only at first call
             return (if null shared
                     then [Prove (InfTreeNode pre cost stmt fn
                                  [(0,"",
                                    InfTreeNodeView (map (second toADatatypeVector) pre)
                                    (map toACostConditionVector cost)
                                    (second toADatatypeVector $ fromJust stmt))] :cs)
                           p count prob cfs sigs cond v noCfDefSyms]
                     else updateAll shared)
         InfTreeNode [] _ _ _ _ ->
             -- only one more rule can be applied, either it works, or it doesn't
             case updateAll (applyAllInferenceRules' args reachability noCfDefSyms
                             (prob, cfs, sigs, v, cond, c)) of
               [] -> -- prove failed, no more preconditions
                   Left $ nonCompositions p
               x -> return x
         _ -> return $ updateAll (applyAllInferenceRules' args reachability noCfDefSyms
                                   (prob, cfs, sigs, v, cond, c))

    where
      shared = share (prob, cfs, sigs, v, cond, c)
      updateAll = map (\(u,w,x,v',y,z) ->  -- normally apply all rules
                                   Prove (z++cs) p count u w x y v' noCfDefSyms)

      -- This function finds the number of contexts which were not generated
      -- by a composition. This is the number of succeeded contexts from the starting
      -- prove, when given all succeeded contexts so far.
      nonCompositions        :: [InfTreeNode] -> Int
      nonCompositions []     = 0
      nonCompositions (con:cons) = if "composition" `elem` map (\(_,a,_) -> a)  (history con)
                                   then nonCompositions cons
                                   else 1 + nonCompositions cons


applyAllInferenceRules' :: ArgumentOptions
                        -> [(String,Integer)]
                        -> [String]
                        -> (ProblemSig, CfSigs, ASigs,
                            Int, ACondition Int Int, InfTreeNode)
                        -> [(ProblemSig, CfSigs, ASigs,
                            Int, ACondition Int Int, [InfTreeNode])]
applyAllInferenceRules' args reachability noCfDefSyms (t, cfs, sig, nr, cond, c) =

  -- trace ("InfTreeNode: " ++ show (prettyInfTreeNode c)) $

  concatMap (\x -> x (t, cfs, sig, nr, cond, c)) rules
        where
          rules = [ share

                  , function args reachability noCfDefSyms
                                  -- list of inference rules (the order is
                                  -- important) the higher the rule is named,
                                  -- the earlier it gets
                  , identity      -- applied. If the prove can be finished
                                  -- without using
                  , composition   -- rules at the end of the list, the
                                  -- computation will
                  , weak          -- be faster.
                  ]


--
-- InferenceRules.hs ends here
