{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Validation
  ( parseGrammar
  , parseGrammarFromString
  , parseGrammarFromFile
  , parseGrammarFromStdin
  ) where

import Control.Monad ((>=>))
import Data.Bifunctor
import qualified Data.Set as S
import Data.Validation
import EbnfGrammar.Error (Error, Errors(..))
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
validateGrammar :: Gram -> Either Errors Gram
validateGrammar =
  (\gram -> merge [checkUniqueHeads gram, checkUniqueConstructors gram]) >=>
  (\gram -> merge [checkUnusedVocab gram, checkUndefinedNonterminals gram]) >=>
  (\gram -> merge [f checkProductivity gram, f checkNullAmbiguities gram])
  where
    f :: (Gram -> Either Error Gram) -> Gram -> Either Errors Gram
    f g gr = first (Errors . S.singleton) $ g gr
    merge :: [Either Errors Gram] -> Either Errors Gram
    merge = toEither . fmap head . traverse fromEither

------------------------------------------------------------
parseGrammar :: [Token] -> Either Errors Gram
parseGrammar toks = do
  gram <- P.parseGrammar toks
  validateGrammar gram

parseGrammarFromString :: String -> Either Errors Gram
parseGrammarFromString src = do
  gram <- P.parseGrammarFromString src
  validateGrammar gram

parseGrammarFromFile :: FilePath -> IO (Either Errors Gram)
parseGrammarFromFile fp = do
  eGram <- P.parseGrammarFromFile fp
  pure $ do
    gram <- eGram
    validateGrammar gram

parseGrammarFromStdin :: IO (Either Errors Gram)
parseGrammarFromStdin = do
  eGram <- P.parseGrammarFromStdin
  pure $ do
    gram <- eGram
    validateGrammar gram
