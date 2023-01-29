module Main
  ( main
  ) where

import EbnfGrammar.Prettyprinter ()
import EbnfGrammar.Validation (parseGrammarFromStdin)
import Prettyprinter (pretty)

main :: IO ()
main = do
  eGram <- parseGrammarFromStdin
  case eGram of
    Left err -> do
      print $ pretty err
      error "failed"
    Right gram -> print $ pretty gram
