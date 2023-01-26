{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module EbnfGrammar.Scanner
  ( Token
  , TokenType(..)
  , scan
  ) where

import Control.Monad.Except (throwError)
import Data.Data (Data)
import EbnfGrammar.Error (Error(..), Posn(..))
import Text.StdToken (StdToken(..))
}

%wrapper "posn"

scanner :-

\:\:\=	{ mkToken YIELDS }
\:	{ mkToken COLON }
\.\.\.	{ mkToken ELLIPSIS }
\.	{ mkToken FULL_STOP }
\{	{ mkToken LEFT_BRACE }
\[	{ mkToken LEFT_BRACKET }
\|	{ mkToken OR }
\}\+	{ mkToken RIGHT_BRACE_PLUS }
\}	{ mkToken RIGHT_BRACE }
\]	{ mkToken RIGHT_BRACKET }
[a-z][a-z0-9_]*	{ mkToken LOWER_NAME }
[A-Z][A-Z0-9_]*	{ mkToken UPPER_NAME }

$white+  ;
\#.*$ ;

. { scanError }
{
mkToken :: TokenType -> AlexPosn -> String -> Either Error Token
mkToken tt (AlexPn o l c) txt = Right $ Token tt txt (Posn o l c)

scanError :: AlexPosn -> String -> Either Error Token
scanError (AlexPn o l c) txt = throwError $ ScanError (Just $ Posn o l c) txt

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

scan :: String -> Either Error [Token]
scan = sequenceA . alexScanTokens
}
