{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Validation
  ( parseGrammar
  , parseGrammarFromString
  , parseGrammarFromFile
  ) where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Control.Monad.Except
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations
import Data.List (find, foldl', nub, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import EbnfGrammar.Error
import qualified EbnfGrammar.Parser as P
import EbnfGrammar.Posn (Posn)
import EbnfGrammar.Syntax
import EbnfGrammar.Token (Token)
import Text.StdToken

collectOnFirst ::
     forall a b. Ord a
  => [(a, b)]
  -> [(a, [b])]
collectOnFirst abs' = [(a, [b' | (a', b') <- abs', a' == a]) | a <- as']
  where
    as' :: [a]
    as' = sort $ nub $ map fst abs'

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
checkUnusedVocab :: Gram -> Either Error Gram
-- make a graph and check reachable from the root
checkUnusedVocab g@(Gram ps) =
  if S.null unreachables
    then pure g
    else throwError $ UnreachableError unreachables
  where
    unreachables :: S.Set String
    unreachables = allSymbols S.\\ reachables
      where
        allSymbols = vertexSet gr
    Prod hd _ = NE.head ps
    startSym = _tokenText hd
    reachables :: S.Set String
    reachables = S.fromList $ reachable startSym gr
    gr = edges es
    es :: [(String, String)]
    es =
      [ (_tokenText hd', vocabText v)
      | Prod hd' alts <- NE.toList ps
      , Alt _mCtor ts <- NE.toList alts
      , t <- ts
      , v <- termVocab t
      ]
    termVocab :: Term -> [Vocab]
    termVocab (VocabTerm v) = pure v
    termVocab (Opt v) = pure v
    termVocab (Rep0 v) = pure v
    termVocab (Rep1 v) = pure v
    termVocab (Repsep0 b s) = [b, s]
    termVocab (Repsep1 b s) = [b, s]
    vocabText :: Vocab -> String
    vocabText (NT tok) = _tokenText tok
    vocabText (T tok) = _tokenText tok

------------------------------------------------------------
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

------------------------------------------------------------
-- TODO If I've already determined there are no duplicate heads...  I
-- can define Ord via the head.
checkProductivity :: Gram -> Either Error Gram
-- make a table of prods and alts and recurse to a fixed point
checkProductivity g =
  if S.null unproductives
    then pure g
    else throwError $ UnproductiveError unproductives
  where
    unproductives :: S.Set PA
    unproductives = allPA S.\\ productives
    Gram ps = ensureCtors g
    allPA :: S.Set PA
    allPA =
      S.fromList $
      concat
        [ P prod : [A alt | alt <- NE.toList alts]
        | prod@(Prod _hd alts) <- NE.toList ps
        ]
    productives :: S.Set PA
    productives = monotoneFixedPoint calcProductives S.empty
    calcProductives :: S.Set PA -> S.Set PA
    calcProductives = foldl' calcProductive allPA
      where
        calcProductive :: S.Set PA -> PA -> S.Set PA
        calcProductive isProductive pa =
          if pa `S.member` isProductive
            then isProductive
            else let paIsProductive =
                       case pa of
                         P (Prod _hd alts) ->
                           any ((`S.member` isProductive) . A) $ NE.toList alts
                         A (Alt _ terms) -> all isProductiveTerm terms
                  in if paIsProductive
                       then S.insert pa isProductive
                       else isProductive
          where
            isProductiveTerm :: Term -> Bool
            isProductiveTerm (VocabTerm v) = isProductiveVocab v
            isProductiveTerm (Opt _) = True
            isProductiveTerm (Rep0 _) = True
            isProductiveTerm (Rep1 v) = isProductiveVocab v
            isProductiveTerm (Repsep0 _ _) = True
            isProductiveTerm (Repsep1 b _) = isProductiveVocab b
            isProductiveVocab :: Vocab -> Bool
            isProductiveVocab (T _) = True
            isProductiveVocab (NT tok) = isProductiveProdName $ _tokenText tok
            isProductiveProdName :: String -> Bool
            isProductiveProdName str =
              str `elem`
              [_tokenText hd | P (Prod hd _) <- S.toList isProductive]

ensureCtors :: Gram -> Gram
ensureCtors (Gram ps) = Gram $ fmap ensureCtorProd ps
  where
    ensureCtorProd :: Prod -> Prod
    ensureCtorProd (Prod hd alts) = Prod hd $ fmap (ensureCtorAlt hd) alts
    ensureCtorAlt :: Token -> Alt -> Alt
    ensureCtorAlt hd (Alt Nothing ts) = Alt (Just hd) ts
    ensureCtorAlt _ a = a

monotoneFixedPoint ::
     forall a. Eq a
  => (a -> a)
  -> a
  -> a
monotoneFixedPoint f a = maybe undefined fst $ find (uncurry (==)) pairs
  where
    monotoneSeq :: [a]
    monotoneSeq = iterate f a
    pairs = zip monotoneSeq (tail monotoneSeq)

------------------------------------------------------------
checkNullAmbiguities :: Gram -> Either Error Gram
-- figure nullables, check in all the extensions
checkNullAmbiguities g =
  if null ambiguousTerms
    then pure g
    else throwError $ NullAmbiguitiesError ambiguousTerms
  where
    ambiguousTerms :: [Term]
    ambiguousTerms = filter isAmbiguous $ universeBi g
    nullableNTs :: S.Set String
    nullableNTs = nullables g
    isAmbiguous :: Term -> Bool
    isAmbiguous (Opt b) =
      case b of
        NT tok -> _tokenText tok `S.member` nullableNTs
        T _ -> False
    isAmbiguous (Rep0 b) =
      case b of
        NT tok -> _tokenText tok `S.member` nullableNTs
        T _ -> False
    isAmbiguous (Repsep0 b _) =
      case b of
        NT tok -> _tokenText tok `S.member` nullableNTs
        T _ -> False
    isAmbiguous _ = False

nullables :: Gram -> S.Set String
nullables (Gram ps) = monotoneFixedPoint calcNullables S.empty
  where
    calcNullables :: S.Set String -> S.Set String
    calcNullables = flip (foldl' calcNullable) $ NE.toList ps
      where
        calcNullable :: S.Set String -> Prod -> S.Set String
        calcNullable nullables' (Prod hd alts)
          | hdSym `S.member` nullables' = nullables'
          | any isNullableAlt (NE.toList alts) = S.insert hdSym nullables'
          | otherwise = nullables'
          where
            hdSym = _tokenText hd
            isNullableAlt :: Alt -> Bool
            isNullableAlt (Alt _ ts) = all isNullableTerm ts
            isNullableTerm :: Term -> Bool
            isNullableTerm t =
              case t of
                VocabTerm v -> isNullableVocab v
                Rep1 v -> isNullableVocab v
                Repsep1 b _ -> isNullableVocab b
                _ -> True
            isNullableVocab :: Vocab -> Bool
            isNullableVocab v =
              case v of
                T _ -> False
                NT tok -> _tokenText tok `S.member` nullables'

------------------------------------------------------------
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
