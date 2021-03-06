-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Nov  1 10:05:01 2014 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun May  7 18:03:47 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 69
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


{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}

#define DEBUG


module Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Ops
    (

    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.Exception

import           Control.Exception                                         (throw)
#ifdef DEBUG
import           Debug.Trace                                               (trace)
#endif


instance (Eq a, Eq dt) => Eq (ADatatype dt a) where
  (ActualCost isCf1 dtN1 cst1) == (ActualCost isCf2 dtN2 cst2) =
    -- isCf1 == isCf2 &&
    dtN1 == dtN2 && cst1 == cst2
  (SigRefRet _ nr1) == (SigRefRet _ nr2) = nr1 == nr2
  (SigRefParam _ nr11 nr12) == (SigRefParam _ nr21 nr22) = nr11 == nr21 && nr12 == nr22
  (SigRefParamCf _ nr11 nr12) == (SigRefParamCf _ nr21 nr22) = nr11 == nr21 && nr12 == nr22
  -- _ == _  = False
  _ == _ = throw $ FatalException $ "Programming error. You need to fetch " ++
            "the signature first before comparing the values."


instance (Show a, Ord a, Eq dt) => Ord (ADatatype dt a) where
  (ActualCost isCf1 n1 c1) <= (ActualCost isCf2 n2 c2) =
    if n1 /= n2
    then throw $ FatalException "Cannot compare different data-types on costs."
    else c1 <= c2
  _ <= _ = throw $ FatalException "Cannot compare SigRef*'s."

--
-- Ops.hs ends here
