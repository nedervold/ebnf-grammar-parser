{-# LANGUAGE DeriveDataTypeable #-}

module EbnfGrammar.Error
  ( Posn(..)
  , Error(..)
  ) where

import Data.Data (Data)
import qualified Data.Set as S

data Posn =
  Posn
    { charOffset :: !Int
    , lineNumber :: !Int
    , columnNumber :: !Int
    }
  deriving (Data, Eq, Ord, Show)

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
  | UndefinedNTs (S.Set String)
  deriving (Show)
