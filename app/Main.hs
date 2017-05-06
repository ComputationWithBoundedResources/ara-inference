{-# LANGUAGE FlexibleContexts #-}
-- Main.hs ---
--
-- Filename: Main.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 10:19:05 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat May  6 18:37:09 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 941
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
-- This is the main starting point of the application.

-- Usage help text:
--     $ ./amortized-cost-analysis-by-inference-rules -h

-- GHCI argument passing:
--     *Main> :main -h -v filePath.trs
--
-- or with set, e.g. when used :trace to DEBUG:
--     *Main> :set args ../Examples/list.trs --debug -v
--     *Main> :trace main
--


-- To enabl/disable DEBUG output:
-- compile/'start ghci' with -DDEBUG and run with --debug
--
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


{-# LANGUAGE CPP              #-}
module Main
    ( main
    ) where

#ifdef DEBUG
import           Control.Monad                                             (when)
import           Debug.Trace                                               (trace)
#endif
import           Data.Rewriting.ARA.ByInferenceRules.Analyzer
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT
import           Data.Rewriting.ARA.ByInferenceRules.Graph.Ops
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.InfTreeNode
import           Data.Rewriting.ARA.ByInferenceRules.Prove

import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants                              (seperatorDoc)
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.ARA.Exception.Pretty                       ()
import           Data.Rewriting.ARA.InferTypes
import           Data.Rewriting.ARA.Pretty
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature

import           Control.Arrow                                             hiding
                                                                            ((<+>))
import qualified Control.Exception                                         as E
import           Control.Monad.State
import           Data.Function
import           Data.List
import           Data.Maybe
import           System.Exit                                               (exitFailure)
import           Text.PrettyPrint

import           Debug.Trace

main :: IO ()
main =
  E.catch
       (do                      -- Read arguments
         maybeArgs <- parseArgOpts :: IO (Maybe ArgumentOptions)
         let args = fromMaybe
               (E.throw $ FatalException "Command line Arguments where empty")
               maybeArgs

         when (minVectorLength args > maxVectorLength args)
           (E.throw $ FatalException
            "Minumum vector length was greater than maximum vector length." )


         probFile <- parseFileIO (filePath args)

         -- if no types given, infer them
         let prob = if isNothing (datatypes probFile) || isNothing (signatures probFile)
               then inferTypesAndSignature probFile
               else probFile

         -- Find out SCCs
         let reachability = analyzeReachability prob

         (prove, infTrees) <- analyzeProblem args reachability prob

         when (verbose args) $ do
           putStrLn "Parsed Typed Term Rewrite System:\n"
           print (prettyProve prove)

         -- Solve cost constraints
         let cond = conditions prove
         let probSig = signatures (problem prove)

         when (lowerbound args) $
           E.throw $ FatalException "Lowerbound analysis not yet implemented!"

         when (isNothing probSig && shift args) $
           E.throw $ FatalException "Shift requires signature information in input TRS."


         -- Solve datatype constraints
         (sigs, cfSigs, valsNs, vals, baseCtrs, cfBaseCtrs, bigO, (strictRls, weakRls)) <-
           solveProblem args (fromJust probSig) cond (signatureMap prove) (costFreeSigs prove)


         let line = text "\n"
         let documentsIT :: [(String, Doc)]
             documentsIT = (map (second (\d -> line $+$ d $+$ line)))
                           (map (second $
                                 ((vcat . intersperse seperatorDoc) .
                                  map prettyInfTreeNodeView) . reverse) infTrees)
             documentsITNum = (map (second (\d -> line $+$ d)))
                              (map (second (((vcat . intersperse seperatorDoc) .
                                     map prettyInfTreeNodeView) . reverse)) $
                                zip (map fst infTrees)
                                (putValuesInInfTreeView (signatureMap prove)
                                 (costFreeSigs prove) vals (map snd infTrees)))

             ruleNames :: [(String, Rule String String)]
             ruleNames = map (\rule -> (((\(Fun f _) -> f) . lhs) rule, rule)
                             ) (allRules $ rules prob)

             polyIntASigs :: [(Rule String String, ASignatureSig)]
             polyIntASigs = fst $ foldl fun ([],ruleNames) sigs

             fun (acc,[]) _ = (acc,[])
             fun (acc,(rn, rule):rns) sig = if rn == fst4 (lhsRootSym sig)
                                            then  (acc ++ [(rule,sig)], rns)
                                            else (acc, (rn,rule):rns)


         when (printInfTree args) $ do
           let grpBy f = groupBy ((==) `on` f) -- . sortBy (compare `on` f)
           let printInfTrees xs = line $+$ text n $+$ text (replicate (length n) '-')
                                  $+$ nest 2 (foldl ($+$) empty docs)

                 where n = fst $ head xs
                       docs = map snd xs
           print (empty $+$ empty $+$
                   text "Inference Trees:\n----------------")
           print (nest 2 (foldl ($+$) empty (map printInfTrees (grpBy fst documentsIT)))
                   $+$ line)

           print (text "Inference Trees (with filled in numbers):" $+$
                   text "-----------------------------------------")
           print (nest 2 (foldl ($+$) empty (map printInfTrees (grpBy fst documentsITNum)))
                  $+$ line)


         -- print solution
         if lowerbound args
           then putStrLn $ "WORST_CASE(O(n^" ++ show bigO ++ "),?)\n"
           else putStrLn $ "WORST_CASE(?,O(n^" ++ show bigO ++ "))\n"

         print (text "Solution:\n" <> text "---------\n")
         print $ vcat $
           zipWith (\nr x -> (if printInfTree args
                             then nest 2 $ int nr <> colon
                             else empty)
                             <+> prettyAraSignature' x) [0..]
           ( -- concatMap (sortBy (compare `on` rhsSig)) $
             -- groupBy ((==) `on` fst4 . lhsRootSym) $
             if printInfTree args
             then sigs
             else sortBy (compare `on` fst4 . lhsRootSym) (nub sigs))

         -- Cost free signatures
         print (text "\n\nCost Free Signatures:\n" <> text "---------------------\n")
         print $ vcat $
           zipWith (\nr x -> (if printInfTree args
                              then nest 2 $ int nr <> colon
                              else empty)
                             <+> prettyAraSignature' x) [0..]

           ( -- concatMap (sortBy (compare `on` rhsSig)) $
             -- groupBy ((==) `on` fst4 . lhsRootSym) $
             if printInfTree args
             then cfSigs
             else sortBy (compare `on` fst4 . lhsRootSym) (nub cfSigs))

         -- e <- trace "HERE" $ checkCfBaseCtrsUniqueness cfSigs -- check uniqueness
         -- print e


         unless (shift args) $
           print (line <>
                  text "\nBase Constructors:\n------------------"  $+$ empty $+$
                  (vcat (map prettyAraSignature'
                         (if printInfTree args
                          then baseCtrs
                          else sortBy (compare `on` fst4 . lhsRootSym) (nub baseCtrs))))
                  <> line)

         unless (null cfBaseCtrs) $
             print (text "\nBase Constructors for Cost-free Signatures:" <>
                    text "\n-------------------------------------------"
                    $+$ empty $+$
                    (vcat (map prettyAraSignature'
                           (if printInfTree args
                            then cfBaseCtrs
                            else sortBy (compare `on` fst4 . lhsRootSym) (nub cfBaseCtrs))))
                    <> line)

         when (isJust $ findStrictRules args) $ do
           putStrLn $ "Strict Rules: " ++ show strictRls
           putStrLn $ "Weak Rules: " ++ show weakRls


         ) (\e ->
              case (e :: ProgException) of
                ShowTextOnly txt -> do
                  putStrLn "MAYBE"
                  putStrLn txt
                WarningException txt -> do
                  putStrLn "MAYBE"
                  putStrLn txt
                TimeoutException txt -> do
                  putStrLn "TIMEOUT"
                  putStrLn txt
                FatalException txt -> do
                  putStr "ERROR:"
                  putStrLn txt
                  exitFailure
                ParseException txt -> do
                  putStr "ERROR:"
                  putStrLn txt
                  exitFailure
                UnsolveableException txt -> do
                  putStrLn "MAYBE"
                  putStrLn "UNSAT"
                  putStrLn txt
                SemanticException txt -> do
                  putStr "ERROR:"
                  putStrLn txt
                  exitFailure)


checkCfBaseCtrsUniqueness :: (Show a, Eq a, Ord a, Eq t, Num t, Show t, Show t3) =>
                             [Signature (a, ACost t, Bool, t3) (ADatatype t)] -> IO Bool
checkCfBaseCtrsUniqueness sigs = do
  let grouped = groupBy ((==) `on` fst4.lhsRootSym) $
                sortBy (compare `on` fst4.lhsRootSym) (trace ("HEREasdf") sigs)
  let unique = all areUnique grouped
  return unique

  where areUnique []     = True
        areUnique (x:xs) = all (x `linComb`) xs
        linComb (Signature (_,_,True,_) _ _) _ = True
        linComb x@(Signature (_,ACost xCst,_,_) xLhs xRhs)
          y@(Signature (_,ACost yCst,_,_) yLhs yRhs) =
          let getCst (ActualCost _ _ (ACost nr)) = nr
              xTuple = map getCst xLhs ++ [xCst, getCst xRhs]
              yTuple = map getCst yLhs ++ [yCst, getCst yRhs]
              x' = map (* getCst yRhs) xTuple
              y' = map (* getCst xRhs) yTuple
          in

            trace ("x': " ++ show x' ++ " == " ++ show y' ++ " :y'")
            trace ("xTuple: " ++ show xTuple ++ " == " ++ show yTuple ++ " :yTuple")

            x' == y' ||
             E.throw (FatalException $
                      "Cost free defined functions not unique: " ++ show xTuple ++
                      " /= " ++ show yTuple)


--
-- Main.hs ends here
