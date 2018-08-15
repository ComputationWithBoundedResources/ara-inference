{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- SMT.hs ---
--
-- Filename: SMT.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat May 21 13:53:19 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Wed Aug 15 10:37:12 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 2036
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
--
--

-- Code:

module Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT
    ( solveProblem
    ) where


import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.Heuristic
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.Inserts
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.ConvertSolutionToData
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.ConvertToSMTProblem
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.IO
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.ParseSolutions
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.Type

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.Data.Type
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature

import           Control.Arrow                                                                  hiding
                                                                                                 ((+++))
import           Control.Concurrent
import           Control.Exception                                                              as E
import           Control.Lens                                                                   hiding
                                                                                                 (use)
import           Control.Monad
import qualified Control.Monad.Parallel                                                         as Par
import           Control.Monad.State
import           Control.Parallel.Strategies
import           Data.Function
                                                                                                 (on)
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict                                                                as M
import           Data.Maybe
                                                                                                 (catMaybes,
                                                                                                 fromJust,
                                                                                                 fromMaybe,
                                                                                                 isJust,
                                                                                                 isNothing)
import qualified Data.Set                                                                       as S

import qualified Data.Text                                                                      as T
import           Debug.Trace
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec                                                  hiding
                                                                                                 (try)
import           Text.PrettyPrint                                                               hiding
                                                                                                 (empty)

use :: ArgumentOptions -> SMTProblem
use args =
  case smtSolver args of
    Z3      -> z3 logic timeo
    MiniSMT -> minismt logic timeo
  where  logic
           | shift args = "QF_LIA"
           | otherwise = "QF_NIA"
         timeo = ((*) mul . fromIntegral) <$> timeout args
         mul | allowCf args = max 0.5 $ min 0.80 (1-ruleWeight * nrRules)
             | otherwise = max 0.5 $ min 0.95 (1-ruleWeight * nrRules)
         nrRules = fromIntegral $ fromMaybe 6 (nrOfRules args)
         ruleWeight = 0.0070    -- in percent!

z3 :: T.Text -> Maybe Double -> SMTProblem
z3 logic timeo =
  emptySMTProblem "z3" logic declareAsConst True
  (["-T:" `T.append` T.pack (show $ fromJust timeo) | isJust timeo ] ++ ["-smt2"])
  parseZ3


minismt :: T.Text -> Maybe Double -> SMTProblem
minismt logic timeo =
  emptySMTProblem "minismt" logic declareAsFun False
  (["-t " `T.append` T.pack (show $ fromJust timeo) | isJust timeo ] ++ ["-v2", "-m", "-neg"])
  parseMinismt


emptySMTProblem :: T.Text
                -> T.Text
                -> (T.Text -> T.Text)
                -> Bool
                -> [T.Text]
                -> Parser [(String, Int)]
                -> SMTProblem
emptySMTProblem name logic declFun getVals =
  SMTProblem logic declFun getVals S.empty S.empty [] [] [] M.empty name

declareAsConst n = "(declare-const " +++ n +++ " Int)\n"
declareAsFun n = "(declare-fun " +++ n +++ " () Int)\n"

doFork :: IO (Either a b) -> IO (IORef (ThreadState a b), ThreadId)
doFork f = do
  ref <- newIORef NotReady
  threadId <- forkIO (f >>= writeIORef ref . Ready)
  return (ref,threadId)

data ThreadState a b = NotReady | Ready (Either a b)


instance Functor (ThreadState a) where
  fmap f NotReady   = NotReady
  fmap f (Ready ei) = Ready $ fmap f ei


solveProblem :: (Eq s, Eq sDt, Ord s, Show s, Show dt, Ord dt, Show f, Show v) =>
             ArgumentOptions
             -> [SignatureSig s sDt]
             -> ACondition f v Int Int
             -> ASigs dt s
             -> CfSigs dt s
             -> IO ([ASignatureSig s dt], [ASignatureSig s dt], [Data Int], M.Map String Vector
                  , [ASignatureSig String dt], [ASignatureSig String dt], Int
                  , ([Rule f v],[Rule f v]))
solveProblem ops probSigs conds aSigs cfSigs = do
  let maxNrVec = maxVectorLength ops
  let minNrVec = minVectorLength ops
  let eqZero
        | isJust (lowerboundArg ops) = []
        | otherwise = concatMap constantToZero (zip [0 ..] (map fst3 aSigs) ++ zip [0 ..] (map fst3 cfSigs))
  let probNoShift = execState (addEqZeroConstraints eqZero) (use ops {shift = False})
  let probShift = execState (addEqZeroConstraints eqZero) (use ops {shift = True})
  let vecLens = [minNrVec .. maxNrVec]
  when (lowerbound ops && maxNrVec > 1) (throw $ FatalException "vector length for this lowerbound method must be 1")
  when (maxNrVec < 1 || maxNrVec > maximumVectorLength) (throw $ FatalException $ "vector length must be in [1.." ++ show maximumVectorLength ++ "]")
  let isLower = lowerbound ops || isJust (lowerboundArg ops)
  let handler (e :: ProgException) = return $ Left e
  solsHeur <-
    sequenceA $
    parMap
      rpar -- with heuristics
      (\nr -> doFork $ E.handle handler (Right <$> evalStateT (solveProblem' (ops {shift = True}) probSigs conds aSigs cfSigs nr) probShift))
      (if lowerbound ops
         then [1]
         else (if isLower
                 then reverse
                 else id)
                vecLens)
  sols <-
    sequenceA $
    parMap -- without heuristics
      rpar
      (\nr ->
         if shift ops
           then doFork $ return $ Left $ FatalException "Shift disabled. Should not be called." -- just here to ensure it is not evaluated
           else doFork $ E.handle handler (Right <$> evalStateT (solveProblem' (ops {shift = False}) probSigs conds aSigs cfSigs nr) probNoShift))
      (if lowerbound ops
         then [1]
         else (if isLower
                 then reverse
                 else id)
                vecLens)
  let getSol xss@(((xIoRef,_), h):xs) = do
        xState <- readIORef xIoRef
        case xState of
          NotReady -> yield >> getSol xss
          -- threadDelay
          Ready x ->
            case x of
              Left e ->
                if null xs
                  then throw e
                  else getSol xs
              Right x -> return x
  let ls h nh
        | shift ops = [h]
        | isLower = [h, nh]
        | otherwise = [h, nh]
  let allSols =
        (if shift ops
           then filter snd
           else id) $
        concat $ zipWith ls (zip solsHeur (repeat True)) (zip sols (repeat False))
  getSol allSols

baseCtrSigDef x y = fst4 (lhsRootSym x) == fst4 (lhsRootSym y) &&
                    getDt (rhsSig x) == getDt (rhsSig y)


solveProblem' :: (Num a, Ord a, Show a, Show a1, Show s, Eq s, Ord s, Eq sDt,
                  Show dt, Eq dt, Ord dt, Show f, Show v) =>
                 ArgumentOptions
              -> [SignatureSig s sDt]
              -> ACondition f v a a1
              -> ASigs dt s
              -> CfSigs dt s
              -> Int
              -> StateT SMTProblem IO ([ASignatureSig s dt], [ASignatureSig s dt], [Data Int],
                                       M.Map String Vector, [ASignatureSig String dt],
                                       [ASignatureSig String dt], Int,
                                       ([Rule f v], [Rule f v]))
solveProblem' ops probSigs conds aSigsTxt cfSigsTxt vecLen' = do

  let lowerb = lowerbound ops || isJust (lowerboundArg ops)

  let aSigs = map fst3 aSigsTxt
  let cfSigs = map fst3 cfSigsTxt


  let nonZeroDts = concatMap nonZeroDatatypes (zip [0..] aSigs)

  let vecLen | vecLen' == 0 = 1
             | otherwise = vecLen'


  let constr = nubBy baseCtrSigDef $ filter (thd4 . lhsRootSym) (aSigs++cfSigs)

  when lowerb $ do
    let retEqZero = concatMap retDefFunToZero (zip [0..] aSigs ++ zip [0..] cfSigs)
    addRetEqZeroConstraints vecLen retEqZero
    addAnyNonZeroConstraints vecLen' nonZeroDts -- including 0, thus vecLen'
    let minNrArgs | isJust (lowerboundArg ops) = fromJust (lowerboundArg ops)
                  | otherwise = 1

    -- for main function
    let mainArgNotAllZeroConstr = concatMap mainArgNotAllZero (zip [0..] aSigs)
    mapM_ (addArgNotAllZeroConstr False vecLen minNrArgs) mainArgNotAllZeroConstr

    -- and constructors
    let ctrArgNotAllZeroConstr = concatMap ctrArgNotAllZero (zip [0..] aSigs)
    let baseParamsList = map (baseParams ops probSigs vecLen) constr
    if directArgumentFilter ops
      then mapM_ (addArgNotAllZeroConstr True vecLen 1) ctrArgNotAllZeroConstr
      else mapM_ (addArgNotAllZeroBaseCtr vecLen) baseParamsList


  -- add constraints with specified length
  addCostConditions vecLen (costCondition conds)
  addDtConditions vecLen (dtConditions conds)
  addDtIntConditions vecLen (dtConditionsInt conds)
  addShareConditions vecLen (shareConditions conds)

  when (isJust $ findStrictRules ops) $ do
    let nr = fromJust (findStrictRules ops)
    addFindStrictRulesConstraint nr (snd <$> minus1Vars conds)


  if not (shift ops)
    then do -- multiplication-constraints (ctr must be linear combination of base ctr)
            let mConstr = concatMap (toMConstraints ops probSigs) (zip [0..] aSigs ++ zip [0..] cfSigs)
            addMultConstraints vecLen mConstr

            -- bound growth of constructors
            let growthConstraintsBaseCtr = concatMap (toGrowBoundConstraintsBaseCtr ops probSigs vecLen) constr
            let growthConstraints = map (toGrowBoundConstraints ops) (zip [0..] aSigs ++ zip [0..] cfSigs)

            -- needed because addition of base ctr can cause problems
            addConstructorGrowthConstraints ops vecLen growthConstraints
            addConstructorGrowthConstraints ops vecLen growthConstraintsBaseCtr

            when (isJust $ lowerboundArg ops) $ do
              let costGt0Base = concatMap (toConstantsCostsBaseCtr ops vecLen) constr
              let costGt0 = concatMap (toConstantsCosts ops vecLen) (zip [0..] aSigs ++ zip [0..] cfSigs)

              let nonConstCostsGt0Base = concatMap (toNonConstantsCostsBaseCtr ops vecLen) constr


              -- costsGt0 costGt0Base
              -- costsGt0 costGt0
              costsGt0 nonConstCostsGt0Base

              let baseConstrParams = map (constrParamsBaseCtr ops vecLen) constr
              selectOneArgumentPerConstructor ops vecLen baseConstrParams


            -- set max values for base constructors
            let baseCtrs = map (\x -> (convertToSMTStringText (fst4 (lhsRootSym x))
                                      ,thd4 (lhsRootSym x)
                                      ,length (lhsSig x)
                                      ,removeApostrophes $ show $ getDt (rhsSig x))
                               ) constr

            setBaseCtrMaxValues ops probSigs vecLen baseCtrs

    else do let ctrSigs = filter (thd4 . lhsRootSym) probSigs
            let isRecursive x = let rhsDt = fst (rhsSig x)
                                    lhsDts = map fst (lhsSig x)
                                in rhsDt `elem` lhsDts
            let isConstant = null . lhsSig
            let recCtrSigs = filter isRecursive ctrSigs
            let nonRecCtrSigs = filter (not . isRecursive) ctrSigs
            let isLower = lowerbound ops || isJust (lowerboundArg ops)
            let shiftConstr = concatMap (shiftConstraints isLower recCtrSigs nonRecCtrSigs)
                              (zip [0..] aSigs ++ zip [0..] cfSigs)
            addHeuristics vecLen shiftConstr

            let growthConstraints = map (toGrowBoundConstraints ops) (zip [0..] aSigs ++ zip [0..] cfSigs)
            addConstructorGrowthConstraints ops vecLen growthConstraints


  -- set cf groups to ==0 or >0
  let (grpsDt,_) = foldl (groupVars variablesCfDt) ([],-1) (zip [0..] cfSigsTxt)
  let (grpsCst,_) = foldl (groupVars variablesCfCst) ([],-1) (zip [0..] cfSigsTxt)
  let grps = combineGroupVars grpsDt grpsCst
  addCfGroupsConstraints vecLen grps

  let tempDir = tempFilePath ops
  let kf = keepFiles ops
  let shft = shift ops

  sol <- solveSMTProblem shft kf tempDir

  let sortAndGroup = groupBy baseCtrSigDef .
                     sortBy (\x y -> mconcat
                              [ fst4 (lhsRootSym x) `compare` fst4 (lhsRootSym y)
                              , getDt (rhsSig x) `compare` getDt (rhsSig y)])


  let (solVarsNs, solVars) = convertToData vecLen sol
      ctrSigs = map head $ sortAndGroup $ filter (thd4 . lhsRootSym) aSigs
      cfCtrSigs =
        if any (\x -> "rctr" `isInfixOf` show x && "_cf_" `isInfixOf` show x) (M.keys sol)
        then map head $ sortAndGroup $
             filter (thd4 . lhsRootSym) cfSigs
        else []

  let m = M.fromList $ map (\(Data l v) -> (l,v)) solVars

  let min1VarsList = fmap (second (\(AVariableCondition x) -> x)) (minus1Vars conds)
  let retFindStrict
        | isNothing (findStrictRules ops) = ([],[])
        | otherwise =
          let lst = fmap (second (`getValueFromMap` m)) min1VarsList
              strict = map fst $ filter ((==0).snd) lst
              weak = map fst $ filter ((==1).snd) lst
          in (strict,weak)

  let aSigs' = insertIntoSigs aSigs solVars
      fromACost (ACost x) = x
      fromActualCost (ActualCost _ _ x) = x
      hasNonZeroDt (Signature _ lhs (ActualCost _ _ (ACost rhsVec))) =
        isNonZeroVector rhsVec || any (isNonZeroVector.fromACost.fromActualCost) lhs
      isNotConstant = any hasNonZeroDt aSigs'

  return ( aSigs'
         , insertIntoSigs cfSigs solVars
         , solVarsNs
         , m
         , if shift ops then [] else insertIntoSigsCtr ops probSigs vecLen ctrSigs m
         , insertIntoSigsCtr ops probSigs vecLen cfCtrSigs m
         , if isNotConstant then vecLen else 0
         , retFindStrict)


constantToZero :: (Int, Signature (t1, t2, Bool,Bool) t) -> [ACostCondition Int]
constantToZero (nr, Signature (n,_,True,False) [] rhs) = [SigRefCst nr]
constantToZero (nr, Signature (n,_,True,True) [] rhs)  = [SigRefCstCf nr]
constantToZero _                                       = []

retDefFunToZero (nr, Signature (n,_,False,False) _ _)
  | take 4 (filter (/='"') $ show n) == "main" = [SigRefRet "" nr]
  | otherwise = []
retDefFunToZero _                                     = []

nonZeroDatatypes (nr, Signature (n,_,isCtr,False) lhs rhs) =
  zipWith (curry nonZeroParam) [0..] lhs ++ [ nonZeroRet | isCtr]
  where nonZeroParam (pNr,_) = SigRefParam "" nr pNr
        nonZeroRet = SigRefRet "" nr

mainArgNotAllZero :: (Show t, Show s) =>
              (Int, Signature (s, t2, Bool,Bool) t)
           -> [(Int,T.Text,[ADatatype String Int])]
mainArgNotAllZero (nr, Signature (n,_,False,False) lhs rhs)
  | take 4 (filter (/='"') $ show n) == "main" =
      [(nr,convertToSMTStringText n,map (SigRefParam "" nr) [0..length lhs-1])]
  | otherwise = []
mainArgNotAllZero _ = []

ctrArgNotAllZero :: (Show t, Show s) =>
              (Int, Signature (s, t2, Bool,Bool) t)
           -> [(Int,T.Text,[ADatatype String Int])]
ctrArgNotAllZero (nr, Signature (n,_,True,_) lhs rhs) =
  [(nr, name, map (SigRefParam "" nr) [0..length lhs-1])
  | name /= "main"]
  where name = convertToSMTStringText n
ctrArgNotAllZero _ = []


shiftConstraints :: (Eq s, Eq sDt, Show s) =>
                    Bool
                 -> [Signature (s,ACost Int,Bool,Bool) (sDt, [ACost Int])]
                 -> [Signature (s,ACost Int,Bool,Bool) (sDt, [ACost Int])]
                 -> (Int, ASignatureSig s dt)
                 -> [([(ADatatype String Int, Heuristic (ADatatype String Int))],
                      (ACostCondition Int, Heuristic (ADatatype String Int)))]
shiftConstraints isLower recCtrs nonRecCtrs (nr, Signature (n,_,False,isCf) _ _) = []
shiftConstraints False recCtrs nonRecCtrs (nr, Signature (n,_,_,isCf) [] _) = []
shiftConstraints isLower recCtrs nonRecCtrs sig@(nr, Signature (n,_,True,isCf) lhs rhs)
  | null (lhsSig (snd sig)) = [([],(sigRefCst isCf nr, Zero))] -- One (sigRefRet isCf "" nr)))]
  | forceInterl && length lhsDts < 2 =
      throw $ FatalException $
      "Not enough parameter types for interleaving! Constructor: " ++ show n
  | length lhsCount == 1 =
    [(zipWith (curry toShiftPar) [0..] lhsBools,
                             (sigRefCst isCf nr, Diamond (sigRefRet isCf "" nr)))]
  | otherwise =                 -- take the first two recursive occurrences for
                                -- interleaving
      [([(sigRefRet isCf "" nr,
           (Interleaving isLower (sigRefParam isCf "" nr (head lhsNrs2))
             (sigRefParam isCf "" nr (head (tail lhsNrs2)))))]
         ++ map ((\p -> (p, Zero)) . sigRefParam isCf "" nr) lhsRest
       , (sigRefCst isCf nr, if isLower
                             then One (sigRefRet isCf "" nr)
                             else Zero))]
  where ctrSig = fromJust $ find ((==n) . fst4 . lhsRootSym) (recCtrs ++ nonRecCtrs)
        rhsDt = fst (rhsSig ctrSig)
        lhsDts = map fst (lhsSig ctrSig)
        lhsBools = map (== rhsDt) lhsDts
        lhsCount = filter id lhsBools
        lhsNrs = map fst $ filter snd $ zip [0..] lhsBools
        lhsNrs2
          | forceInterl = [0,1] -- take the first two
          | otherwise = take 2 lhsNrs
        lhsRest = map fst (filter (not.snd) $ zip [0..] lhsBools) ++ if forceInterl then [2..length (lhsSig (snd sig))] else drop 2 lhsNrs
        toShiftPar (parNr, isRec)
          | isRec = (sigRefParam isCf "" nr parNr, Shift (sigRefRet isCf "" nr))
          | otherwise = (sigRefParam isCf "" nr parNr, Zero)
        forceInterl = ctrSig `elem` nonRecCtrs


tuples :: [t] -> [(t, t)]
tuples []     = []
tuples [_]    = []
tuples (x:xs) = [(x, b) | b <- xs] ++ tuples xs


toMConstraints :: (Show t, Show dt, Show a, Show s) => ArgumentOptions
               -> [SignatureSig s sDt]
               -> (Int, Signature (s, t, Bool,Bool) (ADatatype dt a))
               -> [((ACostCondition Int, T.Text, ACostCondition Int)
                   ,[(ADatatype String Int, T.Text, ADatatype String Int)])]
toMConstraints _ sigs (_, Signature (n,_,False,_) _ _)   = []
toMConstraints args sigs (nr, Signature (n,_,True,isCf) lhs rhs) =
  [((sigRefCst isCf nr, T.pack ns,
     AVariableCondition $ "kctr_" ++ baseCf ++ convertToSMTString n)
   , zip3
     (sigRefRet isCf "" nr : map (sigRefParam isCf "" nr) [0..length lhs-1])
     (repeat $ T.pack ns)
     ([SigRefVar undefined $ "rctr_" ++ baseCf ++ convertToSMTString n] ++
      map (\lhsNr ->
             SigRefVar undefined $ "pctr_" ++ baseCf ++ convertToSMTString n ++ "_" ++ show lhsNr)
      [0..length lhs-1])
   )]

  where ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"
        cf = if isCf then "cf_" else ""
        ns = "n" ++ cf ++ show nr


-- To bound growth of constructors potentials
toGrowBoundConstraints :: Show s =>
                          ArgumentOptions
                       -> (Int, Signature (s, t, Bool,Bool) a4)
                       -> [(T.Text, ADatatype String Int, Int, ADatatype String Int,
                            ACostCondition Int, ADatatype String Int)]
toGrowBoundConstraints args (_, Signature (_,_,False,_) _ _) = []
toGrowBoundConstraints args (nr, Signature (n,p,True,isCf) lhs _) =
    map (\y ->
           ( T.pack cf +++ T.pack (show nr)
           , sigRefParam isCf "" nr y
           , y
           , SigRefVar undefined $ "rictr_" ++ cf ++ show nr ++ "_" ++ show y ++
             "_" ++ convertToSMTString n
           , sigRefCst isCf nr
           , sigRefRet isCf "" nr)
        ) [0..length lhs-1]

  where cf = if isCf then "cf_" else ""


-- Base constructor looks like: [pctr_l_0 x pctr_l_1] -kctr_l-> rctr_l
-- Output quadruple: (p(3,0),rictr_3_0_s,k(3),r(3))
toConstantsCosts :: (Show dt, Show s) =>
                  ArgumentOptions
               -> Int
               -> (Int, Signature (s, t, Bool,Bool) (ADatatype dt a))
               -> [ACostCondition Int]
toConstantsCosts args vecLen (nr, Signature (n,_,_,isCf) [] rhs) = [sigRefCst isCf nr]
  where ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"
toConstantsCosts _ _ _ = []


toConstantsCostsBaseCtr :: (Show dt, Show s) =>
                  ArgumentOptions
               -> Int
               -> Signature (s, t, Bool,Bool) (ADatatype dt a)
               -> [ACostCondition Int]
toConstantsCostsBaseCtr args vecLen (Signature (n,_,_,isCf) [] rhs) =
  map (\v -> AVariableCondition $ "kctr_" ++ baseCf ++ convertToSMTString n
             ++ "_" ++ show v
      ) [1..vecLen]
  where ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"
toConstantsCostsBaseCtr _ _ _ = []


toNonConstantsCostsBaseCtr :: (Show dt, Show s) =>
                  ArgumentOptions
               -> Int
               -> Signature (s, t, Bool,Bool) (ADatatype dt a)
               -> [ACostCondition Int]
toNonConstantsCostsBaseCtr args vecLen (Signature (n,_,_,isCf) lhs rhs)
  | null lhs = []
  | otherwise =
    map (\v -> AVariableCondition $ "kctr_" ++ baseCf ++ convertToSMTString n
               ++ "_" ++ show v
        ) [1..vecLen]
  where ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"


-- Base constructor looks like: [pctr_l_0 x pctr_l_1] -kctr_l-> rctr_l
-- Output quadruple: (p(3,0),rictr_3_0_s,k(3),r(3))
toGrowBoundConstraintsBaseCtr :: (Show dt, Show s) =>
                                 ArgumentOptions
                              -> [SignatureSig s sDt]
                              -> Int
                              -> Signature (s, t, Bool,Bool) (ADatatype dt a)
                              -> [[(T.Text, ADatatype dt Int, Int, ADatatype dt Int,
                                  ACostCondition Int, ADatatype dt Int)]]
toGrowBoundConstraintsBaseCtr args sigs vecLen (Signature (n,_,_,isCf) lhs rhs)
  | isCf && not (separateBaseCtr args) = []
  | otherwise =
    map (\v ->
         map (\y ->
                ( convertToSMTStringText $ show n ++ "_basectr" ++ cf ++ show v
                , SigRefVar undefined $ "pctr_" ++ baseCf ++ convertToSMTString n ++
                  "_" ++ show y ++ "_" ++ show v
                , y
                , SigRefVar undefined $ "rictr_base_" ++ baseCf ++ convertToSMTString n
                  ++ "_" ++ show y ++ "_" ++ show v
                , AVariableCondition $ "kctr_" ++ baseCf ++ convertToSMTString n
                  ++ "_" ++ show v
                , SigRefVar undefined $ "rctr_" ++ baseCf ++ convertToSMTString n ++
                  "_" ++ show v)
             ) [0..length lhs-1]
      ) [1..vecLen]
  where cf = if isCf then "cf_" else ""
        ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"

constrParamsBaseCtr :: (Show dt, Show s) =>
                       ArgumentOptions
                    -> Int
                    -> Signature (s, t, Bool,Bool) (ADatatype dt a)
                    -> [[ADatatype dt Int]]
constrParamsBaseCtr args vecLen (Signature (n,_,_,isCf) [] rhs) = []
constrParamsBaseCtr args vecLen (Signature (n,_,_,isCf) [_] rhs) = []
constrParamsBaseCtr args vecLen (Signature (n,_,_,isCf) lhs rhs) =
  map (\v ->
         map (\y -> SigRefVar undefined $ "pctr_" ++ baseCf ++ convertToSMTString n ++
                    "_" ++ show y ++ "_" ++ show v
             ) [0..length lhs-1]
      ) [1..vecLen]
  where cf = if isCf then "cf_" else ""
        ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"


baseParams :: (Show dt, Show s) =>
              ArgumentOptions
           -> [SignatureSig s sDt]
           -> Int
           -> Signature (s, t, Bool,Bool) (ADatatype dt a)
           -> [[ADatatype String Int]]
baseParams args sigs vecLen (Signature (n,_,_,isCf) [] rhs) = []
baseParams args sigs vecLen (Signature (n,_,_,isCf) lhs rhs) =
  map (\y ->
         map (\v ->
                 SigRefVar undefined $ "pctr_" ++ baseCf ++ convertToSMTString n ++
                 "_" ++ show y ++ "_" ++ show v
             ) [1..vecLen]
      ) [0..length lhs-1]
  where cf = if isCf then "cf_" else ""
        ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"

baseConstructors :: (Show s, Show dt) => ArgumentOptions
                 -> [SignatureSig s sDt]
                 -> Int
                 -> Signature (s, t, Bool,Bool) (ADatatype dt a)
                 -> [(Int, ADatatype dt Int)]
baseConstructors args sigs vecLen (Signature (n,_,_,isCf) _ rhs)
  | isCf && not (separateBaseCtr args) = []
  | otherwise =
      map (\nr ->
         (nr, SigRefVar undefined $ "rctr_" ++ baseCf ++ convertToSMTString n ++ "_" ++ show nr)
        ) [1..vecLen] -- over all base constructors
  where cf = if isCf then "cf_" else ""
        ctrType = getDt rhs
        baseCf = if isCf && separateBaseCtr args
                 then removeApostrophes (show ctrType) ++ "_cf_"
                 else removeApostrophes (show ctrType) ++ "_"

combineGroupVars :: [([ADatatype dt Int],[ADatatype dt Int])]
                 -> [([ACostCondition Int],[ACostCondition Int])]
                 -> [([ADatatype dt Int],[ACostCondition Int],
                       [ADatatype dt Int],[ACostCondition Int])]
combineGroupVars = zipWith (\(dtMain, dtRest) (cstMain, cstRest) ->
                              (dtMain, cstMain, dtRest, cstRest))


groupVars :: ((Int, CfSig dt s) -> [varType])
          -> ([([varType],[varType])],Int)
          -> (Int, CfSig dt s)
          -> ([([varType],[varType])],Int)
groupVars f ([],lastNr) sig@(sigNr,cfSig) = ([(f sig,[])],snd3 cfSig)
groupVars f (hd@(main,acc):accs,lastNr) sig@(sigNr,cfSig)
  | lastNr == snd3 cfSig = ((main,acc ++ f sig):accs,lastNr)
  | otherwise = ((f sig,[]):hd:accs,snd3 cfSig)


variablesCfDt :: (Int, (Signature s a, t, t2)) -> [ADatatype String x]
variablesCfDt (nr,(sig,_,_)) = SigRefRetCf "" nr :
                             map (SigRefParamCf "" nr) [0..length (lhsSig sig)-1]

variablesCfCst :: (Int, (Signature s a, t, t2)) -> [ACostCondition x]
variablesCfCst (nr,(sig,_,_)) = [SigRefCstCf nr]


--
-- SMT.hs ends here
