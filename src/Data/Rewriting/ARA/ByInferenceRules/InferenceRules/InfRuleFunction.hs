{-# LANGUAGE ScopedTypeVariables #-}
-- InfRuleFunction.hs ---
--
-- Filename: InfRuleFunction.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 15:05:19 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Jul 24 23:38:45 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 1290
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
module Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleFunction
    ( function

    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.Analyzer.StartingProve
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.InfRuleMisc
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode.Type
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.ARA.Pretty
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature
import           Data.Rewriting.Typed.Term                                      hiding
                                                                                 (map)
import qualified Data.Rewriting.Typed.Term                                      as T

import           Control.Arrow
import           Control.Exception                                              (throw)
import           Data.Function                                                  (on)
import           Data.List                                                      (find,
                                                                                 sort,
                                                                                 sortBy,
                                                                                 transpose)
import           Data.Maybe
import           Text.PrettyPrint

import           Debug.Trace


-- | This includes 3 different types of function applications:
-- 1. Application of non-cost free types.
-- 2. Application of cost-free types.
-- 3. Application for the arguments of well-typedness (using Eq instead of Geq).
function :: forall f v dt. (Eq f, Eq dt, Show f, Eq v, Show v, Show dt, Ord v, Read v) =>
            ArgumentOptions
         -> [(f, Integer)]
         -> [f]
         -> (ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
             ACondition f v Int Int, InfTreeNode f v dt)
         -> [(ProblemSig f v f dt dt f, CfSigs dt f, ASigs dt f, Int,
              ACondition f v Int Int, [InfTreeNode f v dt])]
function args reachability noCfDefSyms (prob, cfsigs, asigs, nr, conds,
               InfTreeNode pre cst (Just (Fun f fc, dt))
               i@(fn,ruleStr,isCtrDeriv,startCsts,sigNr,mCfSigIdx) his) =

  [(prob, cfsigs', asigs', nr', conds', InfTreeNode [] cst Nothing i his' : infTreesCf)
  | (fst . rhsSig) sig == dtName &&    -- 1. datatypes need to match
    all isVar fc &&                    -- 2. children are variables
    length pre == length varsRhs &&    -- 3. number of variables correlate
    length pre == length (lhsSig sig)  -- 4. the signature corresponds
  ]

  where sig = getSignatureByNameAndType' (fromJust $ signatures prob) f (getDt dt)
        -- signature of function f

        dtsChld = map fst (lhsSig sig)

        dtName :: dt
        dtName = getDt (fetchSigValue asigs cfsigs $ toADatatypeVector dt)

        varsRhs = concatMap getTermVars fc

        allRls = allRules (rules prob)
        funName (Fun f' _) = f'
        funName _          = error "Lhs root symbol must be a function"

        fRules = filter (\r -> funName (lhs r) == f) allRls


        sigSCCNr name = snd $ (\x -> trace ("name: " ++ show name)
                                trace ("his: " ++ show his)
                                (head x))

                        $ filter ((== name) . fst) reachability
        isInSCCOfStartSig
          | allowLowerSCC args = sigSCCNr f <= sigSCCNr fn
          | otherwise = sigSCCNr f == sigSCCNr fn

        isInNoCfDefSyms = f `elem` noCfDefSyms
        isCfBranch = isJust mCfSigIdx
        mCfSigLst | isJust mCfSigIdx = mCfSigIdx
                  | otherwise = Just []
        nonCfHasCfBranches = not (isConstructor f) && not isCtrDeriv &&
                             not isCfBranch && (f == fn || isInSCCOfStartSig) &&
                             not isInNoCfDefSyms
                             && allowCf args
                             -- && False
                             -- && isNothing (lowerboundArg args)
        cfBranchNeedSig = isCfBranch && (f /= fn || isInSCCOfStartSig)
        newSigToASig = not isCfBranch && (isConstructor f || f /= fn)
        newDefFunSigToASig = newSigToASig && not (isConstructor f)
        newSigToCfSig = (nonCfHasCfBranches || cfBranchNeedSig) &&
                        not recursiveCfDeriv
        recursiveCfDeriv
          | isJust mCfSigIdx = any ((==f) . fst) (fromJust mCfSigIdx)
          | otherwise = False

        emptyACond = ACondition [] [] [] [] []

        (cfsigs',infTreesCf,conds'',nr')
          | not newSigToCfSig = (cfsigs, [], emptyACond,nr)
          | otherwise =
            foldl
            (\(curCfSigs,curInfTrees,curConds,nrIn) (rule@(Rule (Fun f ch) rhs)) ->
              let dts = fromMaybe [] (datatypes prob)
                  sigs = fromMaybe [] (signatures prob)
                  (_, (infTreeNds,newCfSigs,conds',nrO,_)) =
                    createInfTreeNodes (Right cfGrp) True (Just cfSigIdx) args dts sigs True
                    (rule, ([],curCfSigs,emptyACond,nrIn,[]))

                  setCfHistory (InfTreeNode pre cst post (_,_,ctrDer,cstRoot,_,lst) _) =
                    InfTreeNode pre cst post
                    (fn,ruleStr,ctrDer,cstRoot,sigNr, lst)
                    (his ++ [(fst3 (last his) + 1, "function cf"
                             , InfTreeNodeView
                               (map (show *** toADatatypeVectorString) pre)
                               (map toACostConditionVector cst)
                               ((T.map show show *** toADatatypeVectorString) (fromJust post))
                             )])

                  setCfOrigin (InfTreeNode pre cst post (_,_,ctrDer,cstRoot,_,_) his) =
                    InfTreeNode pre cst post
                    (fn,ruleStr,ctrDer,cstRoot,sigNr,Just ((f,cfSigIdx):) <*> mCfSigLst) his
              in (newCfSigs
                 ,curInfTrees ++ (setCfHistory.setCfOrigin) (head infTreeNds) :
                  map setCfOrigin (tail infTreeNds)
                 ,curConds `addConditions` conds'
                 ,nrO)

            ) (cfsigs ++ [(sig2ASig True args sig,cfGrp,"new cfSigs")],[],emptyACond,nr) fRules

        cfSigIdx
          | newSigToCfSig = length cfsigs
          | otherwise = snd $ head (filter ((==f) . fst) (fromJust mCfSigIdx))

        cfGrp
          | not isCfBranch = maximum ((-1) : map snd3 cfsigs)+1
          | otherwise = snd3 (cfsigs !! snd (head $ fromJust mCfSigIdx))


        fromRule = snd3 (asigs !! sigNr)

        asigs' = asigs ++ [(sig2ASig False args sig, fromRule, "new aSigs from root: " ++
                            show sigNr ++ show his) |  newSigToASig ]

        asigIdx | isCfBranch = cfSigIdx       -- use CfSigIdx
                | newSigToASig = length asigs -- use ASigIdx & add new Signature
                | otherwise = baseSigNr       -- use ASigIdx, but baseSigIdx

        isConstructor f =
          case find ((== f) . fst4 . lhsRootSym . fst3) asigs of
            Nothing -> True
            Just x  -> (thd4 . lhsRootSym . fst3) x

        -- get base signature (that is the first signature in the set of
        -- signatures of this function)
        baseSig = fromJust $ find ((== f) . fst4 . lhsRootSym . fst3 . snd) (zip [0..] asigs)
        baseSigNr = fst baseSig

        -- cfBaseSig = find ((== f) . fst4 . lhsRootSym . fst3 . snd) (zip [0..] cfsigs)
        -- cfBaseSigNr = fmap fst cfBaseSig

        nPre = map (\(a,b) -> sigRefParam isCfBranch a asigIdx b) (zip dtsChld [0..])
        nPreVector = map (\(a,b) -> sigRefParam isCfBranch (show a) asigIdx b) (zip dtsChld [0..])

        fromVar (Var n) = n
        fromVar _       = error "programming error. Should not be possible."

        postVarOrder (a,_) =
          snd $ fromMaybe (error "not possible")
          (find ((== a) . fst) (zip (map fromVar varsRhs) [0..]))

        preSorted = sortBy (compare `on` postVarOrder) pre

        getRetCst (InfTreeNode _ _ (Just (_,cst)) _ _) = cst
        getRetCst _                                    = error "should not happen"


        -- create new variable c for costs
        -- (nr', nCVar)
        --   | not isCtrDeriv && (isCfBranch || nonCfHasCfBranches) = getNewVariableName nr 1
        --   | otherwise = (nr, [])

        conds' = conds { costCondition = costCondition conds ++ newCstCond
                                         ++ costCondition conds''
                       , dtConditions = dtConditions conds ++ newDtCond ++ dtConditions conds''
                       , shareConditions = shareConditions conds ++ newShareCond
                                           ++ shareConditions conds''
                       }


        newCstCond =
          -- ensure the costs are not growing too much
          [(cst, if isCtrDeriv then Eq else geq
            , sigRefCst isCfBranch asigIdx :
             [SigRefCstCf cfSigIdx | nonCfHasCfBranches] -- ++
              -- fmap AVariableCondition nCVar
            )] ++
          [([SigRefCst asigIdx], Eq, [SigRefCst baseSigNr]) | newDefFunSigToASig ]

        newDtCond =
          zipWith (\r n -> ([removeDt $ snd r]
                           , if isCtrDeriv then Eq else geq
                           , sigRefParam isCfBranch "" asigIdx n :
                            [SigRefParamCf "" cfSigIdx n | nonCfHasCfBranches])) preSorted [0..]
          ++ [(sigRefRet isCfBranch "" asigIdx : [SigRefRetCf "" cfSigIdx | nonCfHasCfBranches]
              , if isCtrDeriv then Eq else geq
              , [removeDt dt])]
          ++ concatMap (\x ->
                           [([SigRefParam "" asigIdx x], Eq,
                             [SigRefParam "" baseSigNr x])
                           | newDefFunSigToASig ]
                       ) [0..length pre-1]
          ++ [([SigRefRet "" baseSigNr], Eq, [SigRefRet "" asigIdx]) | newDefFunSigToASig ]

        geq | isJust (lowerboundArg args) || lowerbound args = Leq
            | otherwise = Geq

        newShareCond
          | nonCfHasCfBranches =
            (removeDt dt, Eq, [SigRefRet "" asigIdx, SigRefRetCf "" cfSigIdx]) :
            map (\(a,b,c) -> (removeDt $ snd a, Eq, [removeDt b, removeDt c]))
            (zip3 preSorted nPre (map (SigRefParamCf "" cfSigIdx) [0..]))
          | otherwise = []

        his' = his ++ [(fst3 (last his) + 1, "function",
                        InfTreeNodeLeafView
                        (FunSig (show f) nPreVector [sigRefCst isCfBranch asigIdx]
                          (sigRefRet isCfBranch "" asigIdx))
                       (if nonCfHasCfBranches
                         then Just $
                          FunSig (show f)
                          (map (SigRefParamCf "" cfSigIdx) [0..length nPreVector-1])
                          [SigRefCstCf cfSigIdx]
                          (SigRefRetCf "" cfSigIdx)
                         else Nothing))]

function _ _ _ _ = []

--
-- InfRuleFunction.hs ends here

