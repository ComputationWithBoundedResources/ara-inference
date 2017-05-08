-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Oct  1 15:42:45 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon May  8 16:49:35 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 126
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

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
    ( ADatatype (..)
    , toADatatypeVector
    , toADatatypeVectorString
    , toADatatypeStringDt
    , getDt
    , removeDt
    )
    where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type


data ADatatype dt a = ActualCost Bool dt (ACost a) -- ^ was Cf, data-type and costs
                    | SigRefParam dt Int Int -- ^ m n: the same as the n'th element of the m'th sig
                    | SigRefRet dt Int       -- ^ same return data-type as the m'th signature
                    | SigRefVar dt String -- ^ datatype and variable name for share rule
                    | SigRefParamCf dt Int Int -- ^ m n: the same as the n'th element of the m'th sig
                    | SigRefRetCf dt Int       -- ^ same return data-type as the m'th signature

toADatatypeVector :: ADatatype dt Int -> ADatatype dt Vector
toADatatypeVector (ActualCost cf dt (ACost cst)) = ActualCost cf dt (ACost (Vector1 cst))
toADatatypeVector (SigRefRet dt x)     = SigRefRet dt x
toADatatypeVector (SigRefParam dt m n) = SigRefParam dt m n
toADatatypeVector (SigRefVar dt v)  = SigRefVar dt v
toADatatypeVector (SigRefRetCf dt x) = SigRefRetCf dt x
toADatatypeVector (SigRefParamCf dt m n) = SigRefParamCf dt m n


toADatatypeVectorString :: (Show dt) => ADatatype dt Int -> ADatatype String Vector
toADatatypeVectorString = toADatatypeVector . toADatatypeStringDt

toADatatypeStringDt :: (Show dt) => ADatatype dt c -> ADatatype String c
toADatatypeStringDt (ActualCost cf dt c)   = ActualCost cf (removeApostrophes $ show dt) c
toADatatypeStringDt (SigRefRet dt x)       = SigRefRet (removeApostrophes $ show dt) x
toADatatypeStringDt (SigRefParam dt m n)   = SigRefParam (removeApostrophes $ show dt) m n
toADatatypeStringDt (SigRefVar dt v)       = SigRefVar (removeApostrophes $ show dt) v
toADatatypeStringDt (SigRefRetCf dt x)     = SigRefRetCf (removeApostrophes $ show dt) x
toADatatypeStringDt (SigRefParamCf dt m n) = SigRefParamCf (removeApostrophes $ show dt) m n


removeDt :: ADatatype dt t -> ADatatype String t
removeDt (ActualCost cf dt c)  = ActualCost cf "" c
removeDt (SigRefRet _ x)       = SigRefRet "" x
removeDt (SigRefParam _ m n)   = SigRefParam "" m n
removeDt (SigRefVar _ v)       = SigRefVar "" v
removeDt (SigRefRetCf _ x)     = SigRefRetCf "" x
removeDt (SigRefParamCf _ m n) = SigRefParamCf "" m n


getDt :: ADatatype dt t -> dt
getDt (ActualCost _ dt _)    = dt
getDt (SigRefRet dt x)       = dt
getDt (SigRefParam dt m n)   = dt
getDt (SigRefVar dt v)       = dt
getDt (SigRefRetCf dt x)     = dt
getDt (SigRefParamCf dt m n) = dt


instance Show a => Show (ADatatype dt a) where
  show (SigRefRet _ x)        = "r(" ++ show x ++ ")"
  show (SigRefParam _ m n)    = "p(" ++ show m ++ "," ++ show n ++ ")"
  show (SigRefVar dt v)       = v -- ++ ":" ++ dt
  show (SigRefRetCf _ x)      = "r_cf(" ++ show x ++ ")"
  show (SigRefParamCf _ m n)  = "p_cf(" ++ show m ++ "," ++ show n ++ ")"
  show (ActualCost cf dt cst) = show cst -- ++ ":" ++ dt


removeApostrophes :: String -> String
removeApostrophes = filter (/= '"')


--
-- Type.hs ends here
