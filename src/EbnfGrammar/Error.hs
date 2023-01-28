{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Error
  ( Error(..)
  , ErrorType
  , OldError(..)
  ) where

import qualified Data.Set as S
import EbnfGrammar.Posn
import EbnfGrammar.Prettyprinter ()
import EbnfGrammar.Syntax
import Prettyprinter

data ErrorType
  = ScanError'
  | ParseError'
  | DuplicateHeadError'
  | DuplicateConstructorError'
  | UndefinedNonterminalError'
  | UnproductiveError'
  | UnreachableError'
  | AmbiguousNullableError'
  deriving (Eq, Ord, Show)

data Error
  = Error
      { errorPosn :: Posn
      , errorType :: ErrorType
      , errorText :: String
      }
  | OldError OldError
  deriving (Eq, Ord, Show)

instance Pretty Error where
  pretty Error {..} =
    hsep
      [ pretty errorPosn <> ":"
      , pretty (show errorType ++ ":")
      , pretty errorText
      ]

data OldError
  = ScanError
      { scanErrorPosn :: Maybe Posn
      , scanErrorText :: String
      }
  | ParseError
      { parseErrorPosn :: Maybe Posn
      , parseErrorText :: String
      }
  | NonUniqueHeadsError [(String, [Posn])]
  | NonUniqueConstructorsError [(String, [Posn])]
  | UndefinedNonterminalsError (S.Set String)
  | UnproductiveError (S.Set PA)
  | UnreachableError (S.Set String)
  | NullAmbiguitiesError [Term]
  deriving (Show)

instance Eq OldError where
  _ == _ = True

instance Ord OldError where
  compare _ _ = EQ

-- TODO /Much/ more to do.
instance Pretty OldError where
  pretty ScanError {} = "ScanError"
  pretty ParseError {} = "ParseError"
  pretty NonUniqueHeadsError {} = "NonUniqueHeadsError"
  pretty NonUniqueConstructorsError {} = "NonUniqueConstructorsError"
  pretty UndefinedNonterminalsError {} = "UndefinedNonterminalsError"
  pretty (UnproductiveError pas) =
    vsep ["UnproductiveError:", nest 4 (vcat $ map pretty $ S.toList pas)]
  pretty (NullAmbiguitiesError _) = "NullAmbiguitiesError"
