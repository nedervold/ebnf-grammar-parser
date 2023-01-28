{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Validation
  ( parseGrammar
  , parseGrammarFromString
  , parseGrammarFromFile
  , parseGrammarFromStdin
  ) where

import Control.Monad ((>=>))
import EbnfGrammar.Error (Error)
import qualified EbnfGrammar.Parser as P
import EbnfGrammar.Syntax (Gram)
import EbnfGrammar.Token (Token)
import EbnfGrammar.Validation.NullAmbiguities (checkNullAmbiguities)
import EbnfGrammar.Validation.Productivity (checkProductivity)
import EbnfGrammar.Validation.UndefinedNonterminals (checkUndefinedNonterminals)
import EbnfGrammar.Validation.UniqueConstructors (checkUniqueConstructors)
import EbnfGrammar.Validation.UniqueHeads (checkUniqueHeads)
import EbnfGrammar.Validation.UnusedVocab (checkUnusedVocab)

------------------------------------------------------------
-- These checks could be done in parallel.  Could collect them up by position.
validateGrammar :: Gram -> Either Error Gram
validateGrammar =
  checkUniqueHeads >=>
  checkUniqueConstructors >=>
  checkUnusedVocab >=>
  checkUndefinedNonterminals >=> checkProductivity >=> checkNullAmbiguities

------------------------------------------------------------
parseGrammar :: [Token] -> Either Error Gram
parseGrammar toks = do
  gram <- P.parseGrammar toks
  validateGrammar gram

parseGrammarFromString :: String -> Either Error Gram
parseGrammarFromString src = do
  gram <- P.parseGrammarFromString src
  validateGrammar gram

parseGrammarFromFile :: FilePath -> IO (Either Error Gram)
parseGrammarFromFile fp = do
  eGram <- P.parseGrammarFromFile fp
  pure $ do
    gram <- eGram
    validateGrammar gram

parseGrammarFromStdin :: IO (Either Error Gram)
parseGrammarFromStdin = do
  eGram <- P.parseGrammarFromStdin
  pure $ do
    gram <- eGram
    validateGrammar gram
