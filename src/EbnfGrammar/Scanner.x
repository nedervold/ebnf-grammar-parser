{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module EbnfGrammar.Scanner
  ( scan
  ) where

import EbnfGrammar.Error (Error(..), ErrorType(..), Errors, throwErrors)
import EbnfGrammar.Posn (Posn(..))
import EbnfGrammar.Token (Token, TokenType(..))
import Text.Printf (printf)
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
mkToken :: TokenType -> AlexPosn -> String -> Either Errors Token
mkToken tt (AlexPn o l c) txt = Right $ Token tt txt (Posn o l c)

scanError :: AlexPosn -> String -> Either Errors Token
scanError (AlexPn o l c) txt =
  throwErrors $
  Error
    (Posn o l c)
    ScanError'
    (printf "scan error at char %s" $ show $ head txt)

scan :: String -> Either Errors [Token]
scan = sequenceA . alexScanTokens
}
