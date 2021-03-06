{-# LANGUAGE OverloadedStrings #-}
-- Infer.hs ---
--
-- Filename: Infer.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  2 15:34:35 2016 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Oct 27 18:26:56 2019 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 65
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


module Data.Rewriting.ARA.InferTypes
    ( inferTypesAndSignature
    ) where


import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature


import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T
import           Text.PrettyPrint.ANSI.Leijen

import           Debug.Trace

type St f v s sDt = (Problem f v f String String f, M.Map f Int, [f],[f])

inferTypesAndSignature :: (Pretty f, Pretty v, Ord f, Show f, Eq f) =>
                          [f] -> Problem f v s sDt dt f -> Problem f v f String String f
inferTypesAndSignature mustFuns prob =
  let res = execState infer (prob {datatypes = Nothing, signatures = Nothing}, M.empty, [], mustFuns)
   in res ^. _1
    -- trace ("RES: " ++ show mustFuns)

infer :: (Pretty f, Pretty v, Show f, Ord f) => State (St f v s sDt) ()
infer = do
  inferSigs
  inferTypes

getProblem :: State (St f v s sDt) (Problem f v f String String f)
getProblem = do
  st <- get
  return (st^._1)

getMustFunctionSymbols :: State (St f v s sDt) [f]
getMustFunctionSymbols = do
  st <- get
  return (st^._4)


inferSigs :: (Pretty f, Pretty v, Show f, Ord f, Eq f) => State (St f v s sDt) ()
inferSigs = do
  p <- getProblem
  mustFuns <- getMustFunctionSymbols
  let syms = nub $ symbols p

  let termColl m (Var v) = m
      termColl m (Fun f ch) =
        let m' = case M.lookup f m of
              Nothing -> M.insert f (length ch) m
              Just x -> if x == length ch
                       then m
                       else error $ "different number of parameters in function " ++ show f
        in
          -- trace ("ruls: " ++ show (rules p))
          foldl termColl m' ch

  let ruls = allRules (rules p)
  let paramLen = foldl termColl M.empty (map lhs ruls ++ map rhs ruls)

  let definedFuns = nub $ mustFuns ++ map ((\(Fun f _) -> f). lhs) ruls
  let getSig f =
        let pLen = M.findWithDefault 0 f paramLen
        in Signature f (replicate pLen "A") "A"

  let definedFunsSigs = map getSig definedFuns

  (pr , ma, fs, mustFuns) <- get

  put (pr { signatures = Just definedFunsSigs }
      , paramLen
      , filter (`notElem` definedFuns) syms
      , mustFuns)

  -- modify $ _1 %~ (\x -> x { signatures = Just definedFunsSigs })
  -- modify $ _2 .~ paramLen
  -- modify $ _3 .~ filter (`notElem` definedFuns) syms

  -- trace ("problem: " ++ show (prettyWST' p))
  --   trace ("startTerms: " ++ show (startTerms p))
  --   trace ("symbols: " ++ show (symbols p))
  --   trace ("paramLen: " ++ show paramLen)
  --   trace ("definedFuns: " ++ show definedFuns)
  --   trace ("definedFunsSigs: " ++ show definedFunsSigs)
  --   undefined


getParamLens :: State (St f v s sDt) (M.Map f Int)
getParamLens = do
  st <- get
  return (st^._2)

getConstructorNames :: State (St f v s sDt) [f]
getConstructorNames = do
  st <- get
  return (st^._3)

inferTypes :: (Ord f) => State (St f v s sDt) ()
inferTypes = do

  paramLens <- getParamLens
  constrs <- getConstructorNames
  let makeConstructors n =
        let len = M.findWithDefault 0 n paramLens
        in Constructor n (replicate len ConstructorRecursive)
  let dt = Datatype "A" $ map makeConstructors constrs

  modify $ _1 %~ (\x -> x { datatypes = Just [dt]})

  -- trace ("paramLens: " ++ show paramLens)
  --   trace ("constrs: " ++ show constrs)
  --   trace ("datatypes : " ++ show dt)

  --   undefined


--
-- Infer.hs ends here
