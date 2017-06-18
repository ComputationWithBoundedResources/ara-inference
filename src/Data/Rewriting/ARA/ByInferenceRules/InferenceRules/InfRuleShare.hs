{-# LANGUAGE ScopedTypeVariables #-}
-- InfRuleShare.hs ---
--
-- Filename: InfRuleShare.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Sep 14 17:35:09 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat Jun 17 19:32:35 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 454
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

#define DEBUG

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleShare
    ( share
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures

import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import qualified Data.Rewriting.Typed.Term                                      as T


import           Control.Arrow
import           Control.Exception                                              (throw)
import           Data.Function                                                  (on)
import           Data.List                                                      (delete,
                                                                                 find,
                                                                                 foldl',
                                                                                 group,
                                                                                 groupBy,
                                                                                 sort,
                                                                                 sortBy,
                                                                                 (\\))
import           Data.Maybe                                                     (fromMaybe,
                                                                                 isJust)
import           Data.Ord                                                       (compare)

import           Text.PrettyPrint


#ifdef DEBUG
import           Debug.Trace                                                    (trace)
#endif

share :: forall f v dt . (Eq v, Eq dt, Read v, Ord v, Show v, Show dt, Show f) =>
         ArgumentOptions
      -> (ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
           ACondition f v Int Int, InfTreeNode f v dt)
      -> [(ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
            ACondition f v Int Int, [InfTreeNode f v dt])]
share args (prob, cfsigs, asigs, nr, conds, InfTreeNode pre cst (Just (Fun f fc, dt))
        i@(_,_,isCtrDeriv,_,_,_) his) =
  -- trace ("share")

  -- trace ("pre:" ++ show groupedPre)
  -- trace ("post:" ++ show groupedPostVars)
  -- trace ("pre':" ++ show (concat pre'))
  -- trace ("shareConds:" ++ show shareConds)
  -- trace ("post':" ++ show post')
  -- trace ("nr':" ++ show nr')

    -- trace ("help: " ++ show (pretty (InfTreeNode pre cst (Just (Fun f fc, dt)) fn [])))

    -- trace ("\n\nfunctionName: " ++ show f)
    -- -- trace ("varPostGroups: " ++ show varPostGroups)
    -- -- trace ("any ((>1) . length) varPostGroups: " ++ show (any ((>1) . length) varPostGroups))
    -- trace ("before: " ++ show (InfTreeNode pre cst (Just $ (Fun f fc, dt)) fn []))
    -- trace ("after: " ++ show (InfTreeNode (concat pre') cst (Just (post', dt)) fn []))

    -- trace ("varsPost: " ++ show varsPost)
    -- trace ("varsToReplace: " ++ show varsToReplace)
    -- trace ("subs: " ++ show subs)
    -- trace ("groupedPre: " ++ show groupedPre )
    -- trace ("groupedPostVars: " ++ show groupedPostVars)
    -- trace ("SharePre: " ++ show pre)
    -- trace ("SharePre'': " ++ show pre'')
    -- trace ("share..:" ++ show ((zip groupedPre groupedPostVars)))
    -- trace ("varsPost: " ++ show groupedPostVars)
    -- trace ("varspre: " ++ show groupedPre)

  [ (prob, cfsigs, asigs, nr', conds', [InfTreeNode pre'' cst (Just (post', dt)) i his'])
  | any ((>1) . length) varPostGroups
  ]

  where varsPost :: [v]
        varsPost = map (\(Var x) -> x) (concatMap getTermVars fc)

        varPostGroups = group $ sort varsPost

        pre'' = concat pre' ++ filter ((`notElem` varsPost) . fst) pre

        groupedPre :: [(v, ADatatype dt Int)]
        groupedPre =
          -- groupBy ((==) `on` fst) $
          sortBy (compare `on` fst) $ -- grouped pre vars
          filter ((`elem` varsPost) . fst) pre

        groupedPostVars :: [[v]]
        groupedPostVars = group (sort varsPost)

        hisNr | null his = 0
              | otherwise = fst3 (last his) + 1
        his' = his ++ [(hisNr, "share",
                        InfTreeNodeView
                        (map (show *** toADatatypeVectorString) (concat pre'))
                        (map toACostConditionVector cst)
                        (T.map show show post', toADatatypeVectorString dt))]


        conds' = conds { shareConditions = shareConditions conds ++ shareConds }

        geq | isJust (lowerboundArg args) || lowerbound args = Leq
            | otherwise = Geq


        shareConds = foldl shareConds' [] (zip pre pre')
        shareConds' acc (_, [_]) = acc
        shareConds' acc (preDt, postDts) =
          acc ++ [(removeDt (snd preDt)
                  ,if isCtrDeriv then Eq else geq
                  ,map (removeDt . snd) postDts)]

        origPreOrd ((a,_),_) =
          snd $
          fromMaybe (error "should not happen")
          (find ((== a) . fst . fst) (zip pre [0..]))

        (pre', nr') =
          -- trace ("(zip groupedPre groupedPostVars): " ++ show (zip groupedPre groupedPostVars)) $
          foldl createPre' ([], nr) $
          sortBy (compare `on` origPreOrd) -- revert original order
          (zip groupedPre groupedPostVars)


        createPre' :: ([[(v, ADatatype dt Int)]], Int)
                   -> ((v, ADatatype dt Int), [v])
                   -> ([[(v, ADatatype dt Int)]], Int)
        createPre' (p', nrTmp) (pres, [_]) = (p' ++ [[pres]], nrTmp)
        createPre' (p', nrTmp) (pres, posts) = (p' ++ [p''], nrTmp')
          where (p'', nrTmp') = foldl fun ([], nrTmp) posts
                fun :: ([(v, ADatatype dt Int)], Int) -> t -> ([(v, ADatatype dt Int)], Int)
                fun (pres', nr'') _ =
                  (pres' ++ [(read (show varName), SigRefVar dtVar varName)], nr''+1)
                  where varName = varPrefix ++ show nr''
                        dtVar = actCostDt $ fetchSigValue asigs cfsigs (toADatatypeVector $ snd pres)
                        actCostDt (ActualCost _ dt' _) = dt'
                        actCostDt (SigRefVar dt' _) = dt'
                        actCostDt _ = error "should not be possible"

        subs =
          concat $
          zipWith (\a b -> if length b == 1
                          then []
                          else [(fst a, map fst b)]
                  ) (filter ((`elem` varsPost) . fst) pre) pre'


        post' :: Term f v
        post' = Fun f (snd $ foldl putVarsIntoTerm (subs, []) fc)

        putVarsIntoTerm :: ([(v, [v])], [Term f v])
                        -> Term f v
                        -> ([(v, [v])], [Term f v])
        putVarsIntoTerm (vs, acc) (Var v) =
          case find (\x -> v == fst x) vs of
            Nothing -> (vs, acc ++ [Var v])
            Just (x,ls) -> if length ls == 1
                          then (delete (x,ls) vs, acc ++ [Var (head ls)])
                          else ((x, tail ls) : delete (x,ls) vs, acc ++ [Var (head ls)])
        putVarsIntoTerm (vs, acc) (Fun f ch) = (vs', acc ++ [Fun f ch'])
          where (vs', ch') = foldl putVarsIntoTerm (vs, []) ch

share _ _ = []


--
-- InfRuleShare.hs ends here
