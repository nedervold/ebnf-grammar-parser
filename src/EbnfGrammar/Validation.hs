{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Validation
  ( parseGrammarFromString
  , parseGrammarFromFile
  , parseGrammarFromStdin
  ) where

import Control.Monad ((>=>))
import Data.Validation (fromEither, toEither)
import EbnfGrammar.Error (Errors(..))
import qualified EbnfGrammar.Parser as P
import EbnfGrammar.Syntax (Gram)
import EbnfGrammar.Validation.NullAmbiguities (checkNullAmbiguities)
import EbnfGrammar.Validation.Productivity (checkProductivity)
import EbnfGrammar.Validation.UndefinedNonterminals (checkUndefinedNonterminals)
import EbnfGrammar.Validation.UniqueConstructors (checkUniqueConstructors)
import EbnfGrammar.Validation.UniqueHeads (checkUniqueHeads)
import EbnfGrammar.Validation.UnusedVocab (checkUnusedVocab)

------------------------------------------------------------
validateGrammar :: Gram -> Either Errors Gram
validateGrammar =
  (\gram -> merge [checkUniqueHeads gram, checkUniqueConstructors gram]) >=>
  (\gram -> merge [checkUnusedVocab gram, checkUndefinedNonterminals gram]) >=>
  (\gram -> merge [checkProductivity gram, checkNullAmbiguities gram])
  where
    merge :: [Either Errors Gram] -> Either Errors Gram
    merge = toEither . fmap head . traverse fromEither

------------------------------------------------------------
parseGrammarFromString :: FilePath -> String -> Either Errors Gram
parseGrammarFromString fp src =
  P.parseGrammarFromString fp src >>= validateGrammar

parseGrammarFromFile :: FilePath -> IO (Either Errors Gram)
parseGrammarFromFile fp = parseGrammarFromString fp <$> readFile fp

parseGrammarFromStdin :: IO (Either Errors Gram)
parseGrammarFromStdin = parseGrammarFromString "<stdin>" <$> getContents
