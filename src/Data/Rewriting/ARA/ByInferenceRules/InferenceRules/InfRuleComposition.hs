{-# LANGUAGE ScopedTypeVariables #-}
-- InfRuleComposition.hs ---
--
-- Filename: InfRuleComposition.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Sep 16 01:46:07 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Thu Jun 15 18:22:22 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 662
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

#ifndef DEBUG
#define DEBUG
#endif

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleComposition
    ( composition

    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleFunction
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.ARA.Pretty
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature
import           Data.Rewriting.Typed.Term
                                                                                     (isVar)
import qualified Data.Rewriting.Typed.Term                                          as T

import           Control.Arrow
import           Control.Exception
                                                                                     (throw)
import           Data.List
                                                                                     (find,
                                                                                     sort,
                                                                                     sortBy,
                                                                                     zip4)
import           Data.Maybe
                                                                                     (isJust)
import           Data.Maybe
                                                                                     (fromJust,
                                                                                     fromMaybe)
import           Text.PrettyPrint

#ifdef DEBUG
import           Debug.Trace
#endif


composition :: forall f v dt . (Ord dt, Eq f, Read v, Show dt, Show v, Show f, Eq v) =>
               ArgumentOptions
            -> (ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
                ACondition f v Int Int, InfTreeNode f v dt)
            -> [(ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
                 ACondition f v Int Int, [InfTreeNode f v dt])]
composition args (prob, cfsigs, asigs, nr, conds, InfTreeNode pre cst (Just (Fun f fc, dt))
              i@(_,ruleStr,isCtrDeriv,cstsStart,_,_) his) =

 [ (prob, cfsigs, asigs, nr'+1, conds', funParNode : funChildNodes)
 | length pre == length fcVars &&             -- 1. number of variables fit
   any (not . isVar) fc &&                     -- 2. function is not applicable
   and (zipWith (==) preDtSorted fcDtSorted) -- 3. datatypes match
 ]

  where fcVars = concatMap getTermVars fc

        preDtSorted :: [dt]
        preDtSorted = sort (map ((\(_, x) -> actCostDt (fetchSigValue asigs cfsigs x))
                                 . second toADatatypeVector) pre)

        fcDtSorted :: [dt]
        fcDtSorted = sort dtFunChld

        actCostDt (ActualCost _ dt' _) = dt'
        actCostDt (SigRefVar dt' _)    = dt'
        actCostDt _                    = error "should not happen"

        sig = getSig f (getDt dt)          -- signature of function f
        getSig :: f -> dt -> SignatureSig f dt
        getSig = getSignatureByNameAndType' (fromJust $ signatures prob)

        dtFunChld :: [dt]
        dtFunChld = map fst $ concatMap getDtsRhs (zip fc (lhsSig sig))

        getDtsRhs (Var _, dt') = [dt']
        -- getDtsRhs (Fun _ [], dt') = [dt']
        getDtsRhs (Fun f' ch, dt') = concatMap getDtsRhs (zip ch (lhsSig sigF))
          where sigF = getSig f' (fst dt')

        nr' = nr + length fc + length fc

        conds' = ACondition (costCondition conds ++ nCostCond) (dtConditions conds ++ nDtCond)
                   (shareConditions conds ++ nShareCond) (minus1Vars conds)

        nCostCond = [(cst, if isCtrDeriv then Eq else geq,
                      map (AVariableCondition . show) strVarsCost)]
        nDtCond = []
        nShareCond = []

        newVars :: [v]
        newVars = map (read . show . (varPrefix ++) . show) [nr..nr']

        geq | isJust (lowerboundArg args) && lowerbound args = Leq
            | otherwise = Geq


        strVarsCost = drop (length fc) newVars
        strVarsNode = take (length fc) newVars
        -- varsCost = map Var (drop (length fc) newVars)
        varsNode :: [Term f v]
        varsNode = map (Var . read . show) (take (length fc) newVars)


        funParNode :: InfTreeNode f v dt
        funParNode = InfTreeNode funParNodeParams funParNodeCsts funParNodeFunc i hisFunParNode
          where
            funParNodeParams :: [(v, ADatatype dt Int)]
            funParNodeParams = zip strVarsNode (zipWith SigRefVar (map fst $ lhsSig sig)
                                                (map show strVarsNode))
            funParNodeCsts :: [ACostCondition a]
            funParNodeCsts = [AVariableCondition (show $ head strVarsCost)]
            funParNodeFunc = Just (Fun f varsNode, dt)


            hisFunParNode =
              his ++ [(fst3 (last his) + 1, "comp fun",
                        InfTreeNodeView
                        (map (show *** toADatatypeVectorString) funParNodeParams)
                        funParNodeCsts
                        ((T.map show show *** toADatatypeVectorString) (fromJust funParNodeFunc)))]


        funChildNodes = fst $ foldl funChildNodes' ([], pre)
                        (zip4 fc (tail strVarsCost) strVarsNode (map fst $ lhsSig sig))
          where
            funChildNodes' :: ([InfTreeNode f v dt], [(v, ADatatype dt Int)])
                           -> (Term f v, v, v, dt)
                           -> ([InfTreeNode f v dt], [(v, ADatatype dt Int)])
            funChildNodes' (acc, pres) (term, costVar, varName, dtStr) =
              (acc ++ [chldNode], restPres)
                where chldNode :: InfTreeNode f v dt
                      chldNode = -- trace ("his: " ++ show hisChld) $
                        InfTreeNode chldPres [AVariableCondition (show costVar)]
                                   (Just (term, SigRefVar dtStr (show varName))) i hisChld
                      (chldPres, restPres) = foldl fun ([],[]) pres

                      hisChld = his ++ [(fst3 (last his) + 1, "comp chld",
                                         InfTreeNodeView
                                         (map (show *** toADatatypeVectorString) chldPres)
                                         [AVariableCondition (show costVar)]
                                         (T.map show show term
                                         ,SigRefVar (show dtStr) (show varName)))]

                      fun :: ([(v, ADatatype dt Int)], [(v, ADatatype dt Int)])
                          -> (v, ADatatype dt Int)
                          -> ([(v, ADatatype dt Int)], [(v, ADatatype dt Int)])
                      fun (good, bad) var = if fst var `elem` varsTerm
                                              then (good  ++ [var], bad)
                                              else (good, bad ++ [var])

                      varsTerm = map fromVar $ getTermVars term

            fromVar (Var v) = v

composition _ _ = []


--
-- InfRuleComposition.hs ends here
