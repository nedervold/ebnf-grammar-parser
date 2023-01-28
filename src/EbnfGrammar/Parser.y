{
module EbnfGrammar.Parser
  ( parseGrammar
  , parseGrammarFromString
  , parseGrammarFromFile
  ) where

import Control.Monad.Except(throwError)
import Control.Monad.Reader(Reader, ask, runReader)
import qualified Data.List.NonEmpty as NE
import EbnfGrammar.Error(Error(..))
import EbnfGrammar.Scanner(scan)
import EbnfGrammar.Syntax
import EbnfGrammar.Token(Token, TokenType(..))
import Text.StdToken(StdToken(..))
}

%tokentype { Token }
%token COLON { $$@(Token COLON _ _) }
    ELLIPSIS { $$@(Token ELLIPSIS _ _) }
    FULL_STOP { $$@(Token FULL_STOP _ _) }
    LEFT_BRACE { $$@(Token LEFT_BRACE _ _) }
    LEFT_BRACKET { $$@(Token LEFT_BRACKET _ _) }
    LOWER_NAME { $$@(Token LOWER_NAME _ _) }
    OR { $$@(Token OR _ _) }
    RIGHT_BRACE { $$@(Token RIGHT_BRACE _ _) }
    RIGHT_BRACE_PLUS { $$@(Token RIGHT_BRACE_PLUS _ _) }
    RIGHT_BRACKET { $$@(Token RIGHT_BRACKET _ _) }
    UPPER_NAME  { $$@(Token UPPER_NAME  _ _) }
    YIELDS { $$@(Token YIELDS _ _) }

%monad { Either Error }
%name parseGrammar gram

%%

gram :: { Gram }
gram : prods { Gram (NE.fromList $1) }

prods :: { [Prod] }
prods : prods prod { $1 ++ [$2] }
     | prod { [$1] }

prod :: { Prod }
prod : LOWER_NAME YIELDS alts FULL_STOP
     { Prod $1 (NE.fromList $ runReader $3 $1) }

alts :: { R [Alt] }
alts : alts OR alt { liftA2 (\x1 x3 ->  x1 ++ [x3]) $1 $3 }
     | alt { fmap pure $1 }

alt :: { R Alt }
alt : opt_lower_name COLON terms { fmap (flip Alt $3) $1 }

opt_lower_name :: { R Token }
opt_lower_name : LOWER_NAME { pure $1 }
    | { ask }

terms :: { [Term] }
terms : terms term { $1 ++ [$2] }
    | { [] }

term :: { Term }
term : vocab { VocabTerm $1 }
    | LEFT_BRACKET vocab RIGHT_BRACKET { Opt $2 }
    | LEFT_BRACE vocab RIGHT_BRACE { Rep0 $2 }
    | LEFT_BRACE vocab RIGHT_BRACE_PLUS { Rep1 $2 }
    | LEFT_BRACE vocab ELLIPSIS vocab RIGHT_BRACE { Repsep0 $2 $4 }
    | LEFT_BRACE vocab ELLIPSIS vocab RIGHT_BRACE_PLUS { Repsep1 $2 $4 }

vocab :: { Vocab }
vocab : LOWER_NAME { NT $1 }
    | UPPER_NAME { T $1 }

{
type R = Reader Token

happyError :: [Token] -> Either Error a
happyError toks =
  throwError $
  if null toks
    then ParseError Nothing "<eof>"
    else ParseError (Just pos) txt
  where
    Token _tt txt pos = head toks

parseGrammar :: [Token] -> Either Error Gram
parseGrammarFromString :: String -> Either Error Gram
parseGrammarFromString src = do
  toks <- scan src
  parseGrammar toks

parseGrammarFromFile :: FilePath -> IO (Either Error Gram)
parseGrammarFromFile fp = parseGrammarFromString <$> readFile fp
}
