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
    Left err -> error $ show $ pretty err
    Right gram -> print $ pretty gram
