{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Sep  5 00:00:04 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr  9 22:53:15 2019 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 2901
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


#define DEBUG

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.Analyzer.Ops
    ( analyzeProblem
    )
    where


import           Data.Function                                              (on)
import           Data.Rewriting.ARA.ByInferenceRules.Analyzer.StartingProve
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.InferenceRules.Ops
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.Prove
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.CompletelyDefined
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Typed.Datatype
import           Data.Rewriting.Typed.Problem
import           Data.Rewriting.Typed.Rule
import           Data.Rewriting.Typed.Signature

#ifdef DEBUG
import           Control.Monad                                              (when)
import           Data.List
import           Data.Rewriting.ARA.Pretty
import           Debug.Trace                                                (trace)
#endif

import           Control.Exception                                          (throw)
import           Data.Char                                                  (isNumber)
import           Data.Maybe                                                 (fromMaybe,
                                                                             isJust)
import           Text.PrettyPrint


analyzeProblem :: forall f v dt . (Eq f, Ord f, Ord dt, Eq v, Eq dt, Read v, Ord v,
                                   Show v, Show dt, Show f, Read f, Read dt) =>
                  ArgumentOptions
               -> [(f,Integer)]
               -> Problem f v f dt dt f
               -> IO (Prove f v f dt dt f, [(String, [InfTreeNodeView])])
analyzeProblem args reachability prob =

  if null (allRules $ rules prob)
   then throw (FatalException "No rewrite rules could be parsed. There is nothing to do.")
   else do let sp0 = checkLhsRules (startingProve args (convertProblem prob))


               cond0 = conditions sp0

               sp = sp0 { conditions = ACondition
                                       (costCondition cond0 ++ map snd3 linearBaseConds)
                                       (dtConditions cond0 ++ map fst3 linearBaseConds ++
                                        concatMap thd3 linearBaseConds)
                                       (dtConditionsInt cond0)
                                       (shareConditions cond0)
                                       (minus1Vars cond0)
                        }

               linearBaseConds :: [(([ADatatype String Int], Comparison, [ADatatype String Int])
                                   ,([ACostCondition Int], Comparison, [ACostCondition Int])
                                   , [([ADatatype String Int], Comparison, [ADatatype String Int])]
                                     )]
               linearBaseConds =
                 concatMap createEqConditions $ filter ((> 1) . length) . groupsCSnd .
                 filter (not . thd4 . lhsRootSym . snd) $ zip [0..]
                 (map fst3 $ signatureMap sp0)
                 where groupsCSnd sig = groupBy (compareFun (==) `on` snd) $
                                        sortBy (compareFun compare `on` snd) sig

               compareFun f (Signature (n1,_,_,_) _ rhs1) (Signature (n2,_,_,_) _ rhs2)  =
                 f n1 n2


               createEqConditions ((nr,x):xs) =
                 map (\(nr2, x2) -> (([SigRefRet "" nr], Eq, [SigRefRet "" nr2])
                                    , ([SigRefCst nr], Eq , [SigRefCst nr2])
                                    , map (\y -> ([SigRefParam "" nr y], Eq, [SigRefParam "" nr2 y]))
                                      [0..length (lhsSig x2)-1]
                                    )) xs
               createEqConditions _ = error "createEqConditions"

#ifdef DEBUG
           when (verbose args) $
             print (text "Starting Proves:" $+$ prettyProve sp)
#endif
           let -- solution :: Prove f v f dt dt cn -- the final solution
               solution = (\x -> x { provenInfTreeNodes = provenInfTreeNodes x }) (analyzeProve Nothing [sp])

           let snd6 (_,x,_,_,_,_) = x

               inferenceTrees :: [(String, [InfTreeNodeView])]
               inferenceTrees = map (\s -> (snd6 (functionName s),
                                     map (\(_,_,c) -> c) (history s)))
                                  (reverse $ provenInfTreeNodes solution)

#ifdef DEBUG
           when (verbose args) $
             print (text "Solution Inference:" $+$ prettyProve solution)
#endif


           return (solution, inferenceTrees)

   where analyzeProve :: Maybe (Prove f v f dt dt f) -> [Prove f v f dt dt f] -> Prove f v f dt dt f
         analyzeProve oldProve [] = throw $ WarningException $
            maybe "No history (no previous proof)" show (prettyProve <$> oldProve) ++
            "\n\nThe Term Rewrite System could not be solved using the inference rules :( \n" ++
            "First of all, check if your input has no errors, e.g. wrong signatures. " ++
            "Then, if there is not error, please file a bug report. History is above!"


         analyzeProve _ (p:ps) =
           -- trace ("length ps: " ++ show (length ps)) $
           -- trace ("current prove: " ++ show (pretty p)) $
           -- apply inference rule to the prove
           case applyInferenceRules args reachability p of
             Left nr -> analyzeProve (Just p) ps -- analyzeProve $ putCtxInFront ps nr
             Right proves ->
                 if null proves
                   then analyzeProve (Just p) ps                -- try other possibilities
                   else -- check for solution - if a we get Just solution, return solution
                        -- otherwise iterate
                        fromMaybe (analyzeProve Nothing (proves++ps)) (getSolution proves)


checkLhsRules :: (Show v) => Prove f v f dt dt f -> Prove f v f dt dt f
checkLhsRules p =
  if all (check . lhs) ((allRules . rules . problem) p)
    then p
    else undefined
  where check (Var n) = throw $ FatalException $
                        "Root symbol of LHS must be a function: "  ++ show n
        check (Fun _ c)  = True -- all (checkLhsTerm p) c

-- checkLhsTerm :: Prove -> Term String String -> Bool
-- checkLhsTerm _ (Var _ )   = True
-- checkLhsTerm p (Fun n ch) =
--   if n `elem` ctrNames
--      then all (checkLhsTerm p) ch
--      else throw $ FatalException $
--             "Function " ++ n ++ " is not a constructor, but used in LHS. "
--             ++ "The theory restricts to constructor TRS only."
--     where ctrs = concatMap constructors (fromMaybe [] ((datatypes . problem) p))
--           ctrNames = map (\(Constructor (n',_) _) -> n') ctrs

-- | @convertProblem prob'@ takes as input a parsed problem @prob'@ and creates
-- a problem with a different type signature, which is needed for analyzing.
convertProblem       :: Problem f v f dt dt f -> ProblemSig f v f dt dt f
convertProblem prob' =
  Problem (startTerms prob') (strategy prob') (theory prob') (convertDt $ datatypes prob')
    (convertSig $ signatures prob') (rules prob') (variables prob') (symbols prob')
    (comment prob')

-- | @startingProve prob'@ generates default starting points of the inference
-- trees for the input problem @prob'@.
startingProve :: (Eq v, Eq f, Eq dt, Show dt, Show f, Show v, Ord v, Read v, Ord f, Read f, Read dt) =>
                 ArgumentOptions -> ProblemSig f v f dt dt f -> Prove f v f dt dt f
startingProve args prob' =
  (insertConstraints args . updateDatatypesChildCost . createCtrSig) proveCD
  where prove0 = Prove [] [] 1 prob' [] [] (ACondition [] [] [] [] []) 0 []

        proveCD | (lowerbound args || isJust (lowerboundArg args)) && not (lowerboundNoComplDef args) = mkCompletelyDefinedConds False prove0
                --  | lowerbound args || isJust (lowerboundArg args) = mkCompletelyDefinedConds True prove0
                | otherwise = prove0


-- | This function takes a list of proves and checks it for the finished and
--   successful proves. It either returns a successful prove, or fails.
getSolution    :: MonadFail m => [Prove f v f dt dt f] -> m (Prove f v f dt dt f)
getSolution [] = fail "No prove was found."
getSolution (p:ps) =
    case p of
      Prove [] _ _ _ _ _ _ _ _ -> return p
      _                        -> getSolution ps


--
-- Ops.hs ends here
