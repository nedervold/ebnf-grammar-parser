{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module EbnfGrammar.Scanner
  ( scan
  ) where

import Control.Monad.Except (throwError)
import EbnfGrammar.Error (Error(..), OldError(..))
import EbnfGrammar.Posn (Posn(..))
import EbnfGrammar.Token(Token, TokenType(..))
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
scanError (AlexPn o l c) txt =
  throwError $ OldError $ ScanError (Just $ Posn o l c) txt

scan :: String -> Either Error [Token]
scan = sequenceA . alexScanTokens
}
