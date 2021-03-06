-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Oct 10 14:08:54 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 22:49:27 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 1332
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

{-# LANGUAGE CPP #-}

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature.Ops
    ( insertSignature
    -- , setEqADt
    , fetchSigValue
    -- , fetchSigValueSig
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature.Type
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.ARA.Pretty
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature

import           Control.Exception                                          (throw)
import           Data.Function                                              (on)
import           Data.List                                                  (delete,
                                                                             find,
                                                                             groupBy,
                                                                             maximumBy,
                                                                             sortBy)
import           Data.Maybe                                                 (fromMaybe)
import           Text.PrettyPrint

-- #ifdef DEBUG
import           Debug.Trace                                                (trace)
-- #end if


insertSignature :: ASignatureSig s dt -> [ASignatureSig s dt] -> [ASignatureSig s dt]
insertSignature = flip (++) . return

sigCst :: ASignatureSig s dt -> ACost Vector
sigCst (Signature x _ _) = (\(_,c,_,_) -> c) x

fetchSig :: [Signature x (ADatatype dt t)] -> ADatatype dt t -> ADatatype dt t
fetchSig sigs (SigRefParam _ m n)   = lhsSig (sigs !! m) !! n
fetchSig sigs (SigRefRet _ nr)      = rhsSig (sigs !! nr)
fetchSig sigs (SigRefParamCf _ m n) = lhsSig (sigs !! m) !! n
fetchSig sigs (SigRefRetCf _ nr)    = rhsSig (sigs !! nr)
fetchSig _ x                        = x


fetchSigValue :: (Eq dt) =>
                 ASigs dt s
              -> CfSigs dt s
              -> ADatatype dt Vector
              -> ADatatype dt Vector
fetchSigValue asigs cfsigs (SigRefParam _ m n) =
    -- trace ("fetchSigValue 1")
    -- trace ("m: " ++ show m)
    -- trace ("n: " ++ show n)
    -- trace ("asigs: " ++ show asigs)
  let asigs' = map fst3 asigs
  in fetchSig asigs' (lhsSig (asigs' !! m) !! n)
fetchSigValue asigs cfsigs (SigRefRet _ nr) =
  -- trace ("fetchSigValue 2")
  let asigs' = map fst3 asigs
  in fetchSig asigs' (rhsSig (asigs' !! nr))
fetchSigValue asigs cfsigs (SigRefParamCf _ m n) =
  -- trace ("fetchSigValue 3")
  -- (if m==4 && n==1
  --  then trace ("m: " ++ show m)
  --       trace ("n: " ++ show n)
  --       trace ("cfsigs: " ++ show cfsigs)
  --  else id)
  let cfsigs' = map fst3 cfsigs
  in fetchSig cfsigs' (lhsSig (cfsigs' !! m) !! n)
fetchSigValue asigs cfsigs (SigRefRetCf _ nr) =
  -- trace ("fetchSigValue 4")
  -- trace ("cfsigs:" ++ show cfsigs)
  -- trace ("nr: " ++ show nr)
  let cfsigs' = map fst3 cfsigs
  in fetchSig cfsigs' (rhsSig (cfsigs' !! nr))
fetchSigValue _ _ x =
  -- trace ("fetchSigValue 5")
  x


fetchCstValue :: [ASignatureSig s dt] -> [ASignatureSig s dt]
              -> ACostCondition Vector -> ACost Vector
fetchCstValue asigs cfsigs (SigRefCst nr)   = sigCst (asigs !! nr)
fetchCstValue asigs cfsigs (SigRefCstCf nr) = sigCst (cfsigs !! nr)
fetchCstValue _ _ (ACostValue b)            = ACost b


equalASig :: (Eq dt) => ASigs dt s -> CfSigs dt s -> ASignatureSig s dt -> ASignatureSig s dt -> Bool
equalASig sigs cfsigs (Signature (_,c0,_,_) lhs0 rhs0)  (Signature (_,c1,_,_) lhs1 rhs1) =
  c0 == c1 && length lhs0' == length lhs1' && and (zipWith (==) lhs0' lhs1') && rhs0' == rhs1'

    where lhs0' = map (fetchSigValue sigs cfsigs) lhs0
          rhs0' = fetchSigValue sigs cfsigs rhs0
          lhs1' = map (fetchSigValue sigs cfsigs)  lhs1
          rhs1' = fetchSigValue sigs cfsigs rhs1


--
-- Ops.hs ends here
