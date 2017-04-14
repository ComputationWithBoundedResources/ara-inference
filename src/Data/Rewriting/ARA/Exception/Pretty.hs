-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 10:42:24 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 14:32:50 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 61
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

module Data.Rewriting.ARA.Exception.Pretty
    ( prettyProgException
    ) where

import           Data.Rewriting.ARA.Constants
import           Data.Rewriting.ARA.Exception.Type
import           Text.PrettyPrint

prettyProgException           :: ProgException -> Doc
prettyProgException ex = text (prefix ex) <> text (getElem ex)
    where
      prefix ShowTextOnly {}        = ""
      prefix SemanticException{}    = exceptionPrefixSemantic ++ " "
      prefix WarningException{}     = exceptionPrefixWarning ++ " "
      prefix FatalException{}       = exceptionPrefixFatal ++ " "
      prefix ParseException{}       = exceptionPrefixParse ++ " "
      prefix UnsolveableException{} = exceptionPrefixUnsolveable ++ " "

      getElem (ShowTextOnly x)         = x
      getElem (SemanticException x)    = x
      getElem (WarningException x)     = x
      getElem (FatalException x)       = x
      getElem (ParseException x)       = x
      getElem (UnsolveableException x) = x

--
-- Pretty.hs ends here
