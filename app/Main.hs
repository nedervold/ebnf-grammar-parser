module Main
  ( main
  ) where

import EbnfGrammar.Prettyprinter ()
import EbnfGrammar.Validation (parseGrammarFromFile)
import Prettyprinter (pretty)

main :: IO ()
main = do
  eGram <- parseGrammarFromFile "EiffelGram.ebnf"
  case eGram of
    Left err -> error $ show err
    Right gram -> print $ pretty gram
