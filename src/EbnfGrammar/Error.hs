{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Error
  ( Error(..)
  ) where

import qualified Data.Set as S
import EbnfGrammar.Posn
import EbnfGrammar.Prettyprinter ()
import EbnfGrammar.Syntax
import Prettyprinter

data Error
  = ScanError
      { scanErrorPosn :: Maybe Posn
      , scanErrorText :: String
      }
  | ParseError
      { parseErrorPosn :: Maybe Posn
      , parseErrorText :: String
      }
  | NonUniqueHeads [(String, [Posn])]
  | NonUniqueConstructors [(String, [Posn])]
  | UndefinedNonterminals (S.Set String)
  | UnproductiveError (S.Set PA)
  | UnreachableError (S.Set String)
  | NullAmbiguitiesError [Term]
  deriving (Show)

-- TODO /Much/ more to do.
instance Pretty Error where
  pretty (UnproductiveError pas) =
    vsep ["UnproductiveError:", nest 4 (vcat $ map pretty $ S.toList pas)]
  pretty _ = "<error>"
