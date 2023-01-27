module EbnfGrammar.Error
  ( Error(..)
  ) where

import qualified Data.Set as S
import EbnfGrammar.Posn
import EbnfGrammar.Syntax

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
