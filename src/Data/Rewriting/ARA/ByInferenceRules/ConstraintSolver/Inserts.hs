{-# LANGUAGE ScopedTypeVariables #-}
-- Inserts.hs ---
--
-- Filename: Inserts.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue May 24 13:30:55 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Thu Aug 16 15:46:07 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 144
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

module Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.Inserts
    ( insertIntoSigs
    , insertIntoSigsCtr
    , getValueFromMap
    ) where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.Type
import           Data.Rewriting.ARA.ByInferenceRules.Data.Type
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Typed.Signature

import           Control.Arrow
import           Control.Exception                                             (throw)
import           Control.Lens                                                  hiding (use)
import           Control.Monad
import           Control.Monad.State
import           Data.Function                                                 (on)
import           Data.List
import qualified Data.Map.Strict                                               as M
import           Data.Maybe                                                    (fromJust,
                                                                                fromMaybe,
                                                                                isNothing)
import           Debug.Trace
import           Text.PrettyPrint                                              hiding
                                                                                (empty)


insertIntoSigs :: [ASignatureSig s dt] -> [Data Vector] -> [ASignatureSig s dt]
insertIntoSigs acc dt =
  -- trace ("m: " ++ show m) $
  map (insertIntoSig m) (zip [0..] acc)
  where m = M.fromList $ map (\(Data l v) -> (l,v)) dt

insertIntoSig :: forall s dt. M.Map String Vector -> (Int, ASignatureSig s dt) -> ASignatureSig s dt
insertIntoSig m (nr, Signature (n,k,b,isCf) lhs (ActualCost _ retDt rhs)) =
  -- trace ("isCf: " ++ show isCf) $
  Signature (n, insertIntoCst isCf m (nr, k), b,isCf)
  (map (\(ActualCost _ dt _, n') ->
         insertIntoADatatype isCf m (dt, show (sigRefParam isCf dt nr n' :: ADatatype dt Vector)))
   (zip lhs [0..]))
  (insertIntoADatatype isCf m (retDt, show (sigRefRet isCf retDt nr :: ADatatype dt Int)))
insertIntoSig _ _ = error "insertIntoSig pattern match fail, this should not have happened"


insertIntoCst :: Bool -> M.Map String Vector -> (Int, ACost Vector) -> ACost Vector
insertIntoCst isCf m (nr, x) =
  -- trace ("label: " ++ show label) $
  ACost (getValueFromMap label m)
  where label = show (sigRefCst isCf nr :: ACostCondition Int)


insertIntoADatatype :: Bool -> M.Map String Vector -> (dt, String) -> ADatatype dt Vector
insertIntoADatatype isCf m (dt, lab) =
  ActualCost isCf dt (ACost $ getValueFromMap lab m)


insertIntoSigsCtr :: (Show dt, Show s) =>
                     ArgumentOptions
                  -> [SignatureSig s sDt]
                  -> Int
                  -> [ASignatureSig s dt]
                  -> M.Map String Vector
                  -> [ASignatureSig String dt]
insertIntoSigsCtr args sigs vecLen acc m =
  concatMap (insertIntoSigCtr args sigs vecLen m) acc


insertIntoSigCtr :: (Show s, Show dt) =>
                    ArgumentOptions
                 -> [SignatureSig s sDt]
                 -> Int
                 -> M.Map String Vector
                 -> ASignatureSig s dt
                 -> [ASignatureSig String dt]
insertIntoSigCtr args sigs vecLen m (Signature (n,k,b,isCf) lhs (ActualCost _ retDt rhs)) =
  -- rictr_15_0_div'
  trace ("re: " ++ show (retDt, "r" ++ cf' ++
                                "(ctr_"++ cf ++ show n ++ "_" ++ show 0 ++ ")"))

  map (\idx ->

         Signature (show n ++ "_" ++ removeApostrophes (show retDt),
                    insertIntoCstCtr m ("k" ++ cf' ++ "(ctr_" ++ cf ++ show n ++ "_" ++
                                           show idx ++ ")", k), b,isCf)
        (map (\(ActualCost isCf dt _, n') ->
                insertIntoADatatypeCtr isCf m
                (dt, "p" ++ cf' ++ "(ctr_" ++ cf ++ show n ++ "_" ++ show n' ++
                     "," ++ show idx ++ ")"))
          (zip lhs [0..]))
        (insertIntoADatatypeCtr isCf m (retDt, "r" ++ cf' ++
                                         "(ctr_"++ cf ++ show n ++ "_" ++ show idx ++ ")")))
  [1..vecLen]
  where cf' = if isCf then "_cf" else ""
        cf = if isCf && separateBaseCtr args
             then removeApostrophes (show retDt) ++ "_cf_"
             else removeApostrophes (show retDt) ++ "_"


insertIntoSigCtr _ _ _ _ _ = error "insertIntoSig pattern match fail, this should not have happened"


insertIntoCstCtr :: M.Map String Vector -> (String, ACost Vector) -> ACost Vector
insertIntoCstCtr m (n, x) = ACost (getValueFromMap n m)

insertIntoADatatypeCtr :: Bool -> M.Map String Vector -> (dt, String) -> ADatatype dt Vector
insertIntoADatatypeCtr isCf m (dt, lab) =
  ActualCost isCf dt (ACost $ getValueFromMap lab m)


getValueFromMap :: String -> M.Map String Vector -> Vector
getValueFromMap = M.findWithDefault 0 . removeApostrophes
-- getValueFromMap str = M.findWithDefault (error $ "searched for:" ++ show str) (removeApostrophes str)


--
-- Inserts.hs ends here
