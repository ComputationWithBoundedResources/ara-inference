-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Feb 18 21:02:32 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 129
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
module Data.Rewriting.ARA.ByInferenceRules.InfTreeNode.Ops
    ( putValuesInInfTreeView
    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerSignature
import           Data.Rewriting.ARA.ByInferenceRules.Data.Type
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode.Type
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.Typed.Signature
import           Data.Rewriting.Typed.Term                             hiding
                                                                        (map)


import           Control.Lens
import           Data.List                                             (find)
import qualified Data.Map                                              as M
import           Debug.Trace

putValuesInInfTreeView :: (Show dt) =>
                          ASigs dt s
                       -> CfSigs dt s
                       -> M.Map String Vector
                       -> [[InfTreeNodeView]]
                       -> [[InfTreeNodeView]]
putValuesInInfTreeView sigs cfsigs vals = map (putValuesInInfTreeView' sigs' cfsigs' vals)
  where sigs' = map (\(a,b,c) -> (showSigName a,b,c)) sigs
        cfsigs' = map (\(a,b,c) -> (showSigName a,b,c)) cfsigs
        showSigName (Signature root l r) =
          Signature root (map toADatatypeStringDt l) (toADatatypeStringDt r)


putValuesInInfTreeView' :: ASigs String s
                        -> CfSigs String s
                        -> M.Map String Vector
                        -> [InfTreeNodeView]
                        -> [InfTreeNodeView]
putValuesInInfTreeView' sigs cfsigs vals = map (putValuesInInfTreeNodeView sigs cfsigs vals)

putValuesInInfTreeNodeView :: ASigs String s
                           -> CfSigs String s
                           -> M.Map String Vector
                           -> InfTreeNodeView
                           -> InfTreeNodeView
putValuesInInfTreeNodeView sigs cfsigs vs (InfTreeNodeView pre csts post) =
  InfTreeNodeView (map (putValuesInPre sigs cfsigs vs) pre) (map (putValuesInCst vs) csts)
    (putValuesInPost sigs cfsigs vs post)
putValuesInInfTreeNodeView sigs cfsigs vs (InfTreeNodeLeafView sig cfSig) =
  InfTreeNodeLeafView (inSig sig) (fmap inSig cfSig)
  where inSig (FunSig n pre csts post) =
          FunSig n (map (snd . (\x -> putValuesInPre sigs cfsigs vs ("", x))) pre)
          (map (putValuesInCst vs) csts) (putValuesInPostLeaf sigs cfsigs vs post)
putValuesInInfTreeNodeView _ _ _ InfTreeNodeLeafEmpty = InfTreeNodeLeafEmpty


putValuesInPre :: ASigs String s
               -> CfSigs String s
               -> M.Map String Vector
               -> (String, ADatatype String Vector)
               -> (String, ADatatype String Vector)
putValuesInPre sigs cfsigs vs (n, dt)   = (n, putValuesInDt sigs cfsigs vs dt)

putValuesInCst :: M.Map String Vector -> ACostCondition Vector -> ACostCondition Vector
putValuesInCst vals (AVariableCondition n) = ACostValue (getValueNr vals n)
putValuesInCst vals (SigRefCst nr) = ACostValue (getValueNr vals n)
  where n = "k(" ++ show nr ++ ")"
putValuesInCst vals (SigRefCstCf nr) = ACostValue (getValueNr vals n)
  where n = "k_cf(" ++ show nr ++ ")"
putValuesInCst vals (ACostValue x) = ACostValue x

putValuesInPost :: ASigs String s
                -> CfSigs String s
                -> M.Map String Vector
                -> (Term String String, ADatatype String Vector)
                -> (Term String String, ADatatype String Vector)
putValuesInPost sigs cfsigs vs (t, dt) = (t, putValuesInDt sigs cfsigs vs dt)


putValuesInPostLeaf :: ASigs String s
                    -> CfSigs String s
                    -> M.Map String Vector
                    -> ADatatype String Vector
                    -> ADatatype String Vector
putValuesInPostLeaf = putValuesInDt


putValuesInDt :: ASigs String s
              -> CfSigs String s
              -> M.Map String Vector
              -> ADatatype String Vector
              -> ADatatype String Vector
putValuesInDt _ _ _ (ActualCost isCf dt cst) = ActualCost isCf dt cst
putValuesInDt _ _ vals (SigRefVar dt n)    = ActualCost False dt (ACost $ getValueNr vals n)
putValuesInDt sigs cfsigs vals x =
  ActualCost fromCf dt (ACost $ getValueNr vals $ show x)
  where (fromCf, dt) = (\(ActualCost isCf x _) -> (isCf,x)) $ fetchSigValue sigs cfsigs x


getValueNr :: M.Map String Vector -> String -> Vector
getValueNr vals label = M.findWithDefault 0 label vals


-- putValuesInACost                   :: [Data Int] -> ACost Int -> ACost Int
-- putValuesInACost vals (ACost x nr) = ACost x nr


--
-- Ops.hs ends here
