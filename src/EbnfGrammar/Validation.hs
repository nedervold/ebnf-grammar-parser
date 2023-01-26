{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Validation
  ( parseGrammar
  , parseGrammarFromString
  , parseGrammarFromFile
  ) where

import Control.Monad.Except
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations
import Data.List (nub, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import EbnfGrammar.Error
import qualified EbnfGrammar.Parser as P
import EbnfGrammar.Scanner (Token)
import EbnfGrammar.Syntax
import Text.StdToken

collectOnFirst ::
     forall a b. Ord a
  => [(a, b)]
  -> [(a, [b])]
collectOnFirst abs' = [(a, [b' | (a', b') <- abs', a' == a]) | a <- as']
  where
    as' :: [a]
    as' = sort $ nub $ map fst abs'

checkUniqueHeads :: Gram -> Either Error Gram
checkUniqueHeads g@(Gram ps) =
  if null multiples
    then pure g
    else throwError $ NonUniqueHeads multiples
  where
    multiples :: [(String, [Posn])]
    multiples = fmap (fmap sort) $ filter isMultiple $ collectOnFirst pairs
    isMultiple :: (s, [p]) -> Bool
    isMultiple = (> 1) . length . snd
    pairs :: [(String, Posn)]
    pairs = [(_tokenText hd, _tokenDeco hd) | Prod hd _ <- NE.toList ps]

checkUniqueConstructors :: Gram -> Either Error Gram
checkUniqueConstructors g@(Gram ps) =
  if null multiples
    then pure g
    else throwError $ NonUniqueConstructors multiples
  where
    multiples :: [(String, [Posn])]
    multiples = fmap (fmap sort) $ filter isMultiple $ collectOnFirst pairs
    isMultiple :: (s, [p]) -> Bool
    isMultiple = (> 1) . length . snd
    pairs :: [(String, Posn)]
    pairs =
      [ (_tokenText ctor, _tokenDeco ctor)
      | Prod hd alts <- NE.toList ps
      , Alt mCtor _ <- NE.toList alts
      , let ctor = fromMaybe hd mCtor
      ]

checkUnusedVocab :: Gram -> Either Error Gram
-- make a graph and check reachable frm the root
checkUnusedVocab = pure

checkUndefinedNTs :: Gram -> Either Error Gram
checkUndefinedNTs g@(Gram ps)
  -- TODO I should be recording positions too
 =
  if S.null undefinedNonterminals
    then pure g
    else throwError $ UndefinedNTs undefinedNonterminals
  where
    defined :: [String]
    altss :: [[Alt]]
    (defined, altss) =
      unzip $
      map (\(Prod hd' alts') -> (_tokenText hd', NE.toList alts')) $
      NE.toList ps
    definedNonterminals :: S.Set String
    definedNonterminals = S.fromList defined
    usedNonterminals :: S.Set String
    usedNonterminals =
      S.fromList
        [_tokenText tok | NT tok <- concatMap universeBi $ concat altss]
    undefinedNonterminals :: S.Set String
    undefinedNonterminals = usedNonterminals S.\\ definedNonterminals

checkProductivity :: Gram -> Either Error Gram
-- make a table of prods and alts and recurse to a fixed point
checkProductivity = pure

checkNullAmbiguities :: Gram -> Either Error Gram
-- figure nullables, check in all the extensions
checkNullAmbiguities = pure

-- These checks could be done in parallel.  Could collect them up by position.
validateGrammar :: Gram -> Either Error Gram
validateGrammar =
  checkUniqueHeads >=>
  checkUniqueConstructors >=>
  checkUnusedVocab >=>
  checkUndefinedNTs >=> checkProductivity >=> checkNullAmbiguities

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
