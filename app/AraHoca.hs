{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- AraHoca.hs ---
--
-- Filename: AraHoca.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 10:19:05 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Jul 23 16:39:30 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 1091
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


{-# LANGUAGE CPP                 #-}
module AraHoca
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

import qualified Data.Rewriting.Applicative.Rule                           as H
import           Data.Rewriting.Applicative.Term                           (ASym (..))
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.Constants                              (seperatorDoc)
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.ARA.Exception.Pretty                       ()
import           Data.Rewriting.ARA.InferTypes
import           Data.Rewriting.ARA.Pretty
import qualified Data.Rewriting.Term.Type                                  as R
import           Data.Rewriting.Typed.Problem                              as TP
import           Data.Rewriting.Typed.Rule                                 as TP
import           Data.Rewriting.Typed.Rule
import qualified Data.Rewriting.Typed.Rules                                as RS
import           Data.Rewriting.Typed.Signature
import           Data.Rewriting.Typed.Term                                 as TP hiding
                                                                                  (map)
import           Data.Rewriting.Typed.Term.Type                            hiding (map)

import           Control.Applicative
import           Control.Arrow                                             hiding ((<+>))
import qualified Control.Exception                                         as E
import           Control.Monad.State                                       hiding ((>=>))
import           Data.Function
import qualified Data.IntMap                                               as IMap
import           Data.List
import           Data.Maybe
import           Data.Maybe                                                (fromJust)
import           Hoca.Data.Symbol
import qualified Hoca.PCF.Core                                             as PCF
import qualified Hoca.PCF.Core.DMInfer                                     as DM
import           Hoca.PCF.Desugar                                          (desugar, desugarExpression)
import           Hoca.PCF.Sugar                                            (Context, expressionFromString,
                                                                            programFromString)
import           Hoca.Problem                                              hiding
                                                                            (Problem (..),
                                                                            TRule)
import qualified Hoca.Problem                                              as H
import           Hoca.Transform                                            as T
import           Hoca.Utils                                                (putDocLn)
import           System.Environment                                        (getArgs)
import           System.Exit                                               (exitFailure)
import           System.Exit                                               (exitFailure,
                                                                            exitSuccess)
import           System.IO                                                 (hPutStrLn,
                                                                            stderr)
import qualified Text.PrettyPrint                                          as P
import qualified Text.PrettyPrint.ANSI.Leijen                              as PP

import           Debug.Trace

main :: IO ()
main = flip E.catch errorFun $ do
  maybeArgs <- parseArgOpts :: IO (Maybe ArgumentOptions)
  let args = fromMaybe (E.throw $ FatalException "Command line Arguments where empty") maybeArgs

  -- Check input
  when (minVectorLength args > maxVectorLength args) $
    E.throw $ FatalException "Minimum vector length was greater than maximum vector length."

  -- from hoca
  let programFromArgs f = do
        s <- readFile f
        case programFromString f s >>= desugar Nothing of
          Left e  -> fail (show e)
          Right p -> return p

  let ppShow x = PP.displayS (PP.renderPretty 0.4 80 x) ""
  let transform = try simplifyATRS >=> toTRS >=> try simplify >=> try compress
      typeProgram p = case DM.infer p of
        Left e   -> fail $ unlines [ppShow $ PP.pretty p, ppShow $ PP.pretty e]
        Right p' -> return p'
      defunctionalizeProgram p = case run defunctionalize p of
        Nothing -> fail "defunctionalization failed"
        Just p' -> return p'
      simplifyAtrs p = case run transform p of
        Nothing -> fail "simplification failed"
        Just p' -> return p'

  -- read and parse file (either as functional program or as (typed) TRS)
  probFile <- (toTypedWST <$> (programFromArgs (filePath args) >>= typeProgram >>=
                               defunctionalizeProgram >>= simplifyAtrs)) <|>
              parseFileIO (filePath args)


  -- if no types given, infer them
  let probParse = if isNothing (datatypes probFile) || isNothing (signatures probFile)
        then inferTypesAndSignature probFile
        else probFile

  -- possibly add main function
  let isMainFun (Fun f _) = take 4 f == "main"
      isMainFun _         = False
  let mainFun = filter (isMainFun.lhs) (allRules $ TP.rules probParse)
  let stricts = strictRules (TP.rules probParse)
  prob <- if (lowerbound args || isJust (lowerboundArg args)) &&
             null mainFun && not (null stricts)
          then do
            let (Fun f ch) = lhs $ last stricts
            let sigF = fromMaybe (E.throw $ FatalException $
                                  "Could not find signature of " ++ show f) $
                       find ((== f).lhsRootSym) (fromJust $ signatures probParse)
            let args = map (\nr -> "x" ++ show nr) [1..length ch]
            let argsStr = intercalate "," args
            let rule = Rule (Fun "main" (map Var args)) (Fun f (map Var args))
            return $ probParse { TP.rules = (TP.rules probParse)
                        { strictRules = strictRules (TP.rules probParse) ++ [rule]}
                               , signatures =
                                 fmap (++ [sigF { lhsRootSym = "main" }])
                                 (signatures probParse)
                               }
          else return probParse


  -- Find out SCCs
  let reachability = analyzeReachability prob

  (prove, infTrees) <- analyzeProblem args reachability prob

  when (verbose args) $ do
    putStrLn "Parsed Typed Term Rewrite System:\n"
    print (prettyProve prove)

  -- Solve cost constraints
  let cond = conditions prove
  let probSig = signatures (problem prove)

  when (isNothing probSig && shift args) $
    E.throw $ FatalException "Shift requires signature information in input TRS."


  -- Solve datatype constraints
  (sigs, cfSigs, valsNs, vals, baseCtrs, cfBaseCtrs, bigO, (strictRls, weakRls)) <-
    solveProblem args (fromJust probSig) cond (signatureMap prove) (costFreeSigs prove)


  let line = P.text "\n"
  let documentsIT :: [(String, P.Doc)]
      documentsIT = (map (second (\d -> line P.$+$ d P.$+$ line)))
                    (map (second $
                          ((P.vcat . intersperse seperatorDoc) .
                           map prettyInfTreeNodeView) . reverse) infTrees)
      documentsITNum = (map (second (\d -> line P.$+$ d)))
                       (map (second (((P.vcat . intersperse seperatorDoc) .
                              map prettyInfTreeNodeView) . reverse)) $
                         zip (map fst infTrees)
                         (putValuesInInfTreeView (signatureMap prove)
                          (costFreeSigs prove) vals (map snd infTrees)))

      ruleNames :: [(String, Rule String String)]
      ruleNames = map (\rule -> (((\(Fun f _) -> f) . lhs) rule, rule)
                      ) (allRules $ TP.rules prob)

      fun (acc,[]) _ = (acc,[])
      fun (acc,(rn, rule):rns) sig = if rn == fst4 (lhsRootSym sig)
                                     then  (acc ++ [(rule,sig)], rns)
                                     else (acc, (rn,rule):rns)


  when (printInfTree args) $ do
    let grpBy f = groupBy ((==) `on` f) -- . sortBy (compare `on` f)
    let printInfTrees xs = line P.$+$ P.text n P.$+$ P.text (replicate (length n) '-')
                           P.$+$ P.nest 2 (foldl (P.$+$) P.empty docs)

          where n = fst $ head xs
                docs = map snd xs
    print (P.empty P.$+$ P.empty P.$+$
            P.text "Inference Trees:\n----------------")
    print (P.nest 2 (foldl (P.$+$) P.empty (map printInfTrees (grpBy fst documentsIT)))
            P.$+$ line)

    print (P.text "Inference Trees (with filled in numbers):" P.$+$
            P.text "-----------------------------------------")
    print (P.nest 2 (foldl (P.$+$) P.empty (map printInfTrees (grpBy fst documentsITNum)))
           P.$+$ line)


  -- print solution
  if lowerbound args || isJust (lowerboundArg args)
    then putStrLn $ "BEST_CASE(Omega(n^" ++ show bigO ++ "),?)\n"
    else putStrLn $ "WORST_CASE(?,O(n^" ++ show bigO ++ "))\n"

  let isMainSig (Signature (n,_,_,_) _ _)
        | take 4 n == "main" = True
        | otherwise = False

  print (P.text "Solution:\n" <> P.text "---------\n")
  print $ P.vcat $
    zipWith (\nr x -> (if printInfTree args
                      then P.nest 2 $ P.int nr <> P.colon
                      else P.empty)
                      P.<+> prettyAraSignature' x) [0..]
    (if printInfTree args
      then sigs
      else filter (\x -> not (null mainFun) || not (isMainSig x)) $
           sortBy (compare `on` fst4 . lhsRootSym) (nub sigs))

  -- Cost free signatures
  print (P.text "\n\nCost Free Signatures:\n" <> P.text "---------------------\n")
  print $ P.vcat $
    zipWith (\nr x -> (if printInfTree args
                       then P.nest 2 $ P.int nr <> P.colon
                       else P.empty)
                      P.<+> prettyAraSignature' x) [0..]

    ( if printInfTree args
      then cfSigs
      else sortBy (compare `on` fst4 . lhsRootSym) (nub cfSigs))

  -- e <- trace "HERE" $ checkCfBaseCtrsUniqueness cfSigs -- check uniqueness
  -- print e


  unless (shift args) $
    print (line <>
           P.text "\nBase Constructors:\n------------------"  P.$+$ P.empty P.$+$
           (P.vcat (map prettyAraSignature'
                  (if printInfTree args
                   then baseCtrs
                   else sortBy (compare `on` fst4 . lhsRootSym) (nub baseCtrs))))
           <> line)

  unless (null cfBaseCtrs) $
      print (P.text "\nBase Constructors for Cost-free Signatures:" <>
             P.text "\n-------------------------------------------"
             P.$+$ P.empty P.$+$
             (P.vcat (map prettyAraSignature'
                    (if printInfTree args
                     then cfBaseCtrs
                     else sortBy (compare `on` fst4 . lhsRootSym) (nub cfBaseCtrs))))
             <> line)

  when (isJust $ findStrictRules args) $ do
    putStrLn $ "Strict Rules: " ++ show strictRls
    putStrLn $ "Weak Rules: " ++ show weakRls


errorFun (e :: ProgException) =
              case e of
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
                  -- putStr "ERROR:"
                  putStrLn txt
                  -- exitFailure

                ParseException txt -> do
                  putStr "ERROR:"
                  putStrLn txt
                  exitFailure
                UnsolveableException txt -> do
                  maybeArgs <- parseArgOpts :: IO (Maybe ArgumentOptions)
                  let args = fromMaybe
                        (E.throw $ FatalException "Command line Arguments where empty")
                        maybeArgs
                  putStrLn $ if isJust (lowerboundArg args) || lowerbound args
                    then  "BEST_CASE(Omega(1),?)"
                    else  "MAYBE"
                  putStrLn "UNSAT"
                  putStrLn txt
                SemanticException txt -> do
                  putStr "ERROR:"
                  putStrLn txt
                  exitFailure


checkCfBaseCtrsUniqueness :: (Show a, Eq a, Ord a, Eq t, Num t, Show t, Show t3) =>
                             [Signature (a, ACost t, Bool, t3) (ADatatype dt t)] -> IO Bool
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


toTypedWST :: (Show v, PP.Pretty f, Eq f, Eq v) => H.Problem f v -> TP.Problem String String s sDt dt cn
toTypedWST p = TP.Problem {
  TP.startTerms = TP.AllTerms
  , TP.strategy = TP.Innermost
  , TP.theory = Nothing
  , TP.rules = TP.RulesPair { TP.strictRules = trs, TP.weakRules = [] }
  , TP.variables = nub (RS.vars trs)
  , TP.symbols = nub (RS.funs trs)
  , TP.comment = Nothing
  , TP.datatypes = Nothing
  , TP.signatures = Nothing
  }
  where
    trs = map (fromARule . theRule . fst) (IMap.elems (H.ruleGraph p))

fromARule :: (Show v, PP.Pretty f) => H.ARule f v -> TP.Rule String String
fromARule (H.Rule aSym v) = TP.Rule (fromATerm aSym) (fromATerm v)


fromATerm :: (Show v, PP.Pretty f) => R.Term (ASym f) v -> TP.Term String String
fromATerm (R.Fun aSym ch) = TP.Fun (fromASym aSym) (map fromATerm ch)
fromATerm (R.Var v)       = TP.Var ("v" <> show v)

fromASym :: (PP.Pretty f) => ASym f -> String
fromASym (Sym f) = show (PP.pretty f)
fromASym App     = error "APP encountered"

--
-- AraHoca.hs ends here
