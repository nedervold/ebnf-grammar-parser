{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module EbnfGrammar.Token
  ( Token
  , StdToken(..)
  , TokenType(..)
  , tokenDeco
  , tokenText
  , tokenType
  ) where

import Data.Data (Data)
import EbnfGrammar.Posn (Posn(..))
import Text.StdToken (StdToken(..), tokenDeco, tokenText, tokenType)

data TokenType
  = COLON
  | ELLIPSIS
  | FULL_STOP
  | LEFT_BRACE
  | LEFT_BRACKET
  | LOWER_NAME
  | OR
  | RIGHT_BRACE
  | RIGHT_BRACE_PLUS
  | RIGHT_BRACKET
  | UPPER_NAME
  | YIELDS
  deriving (Data, Eq, Ord, Show)

type Token = StdToken TokenType String Posn

instance Eq Token where
  Token tt txt _ == Token tt' txt' _ = tt == tt' && txt == txt'
