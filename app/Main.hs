module Main
  ( main
  ) where

import EbnfGrammar.Prettyprinter ()
import EbnfGrammar.Validation (parseGrammarFromFile, parseGrammarFromStdin)
import Prettyprinter (pretty)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  eGram <-
    case args of
      [] -> parseGrammarFromStdin
      ["-"] -> parseGrammarFromStdin
      [fp] -> parseGrammarFromFile fp
      _ -> usage
  case eGram of
    Left err -> do
      print $ pretty err
      error "failed"
    Right gram -> print $ pretty gram

usage :: IO a
usage = do
  progName <- getProgName
  hPutStrLn stderr "Could not understand arguments."
  hPutStrLn stderr $ printf "usage: %s [ filepath | - ]" progName
  exitFailure
