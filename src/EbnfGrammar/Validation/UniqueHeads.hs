{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UniqueHeads
  ( checkUniqueHeads
  ) where

import Control.Monad.Except (throwError)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Posn
import EbnfGrammar.Syntax
import EbnfGrammar.Utils
import Prettyprinter
import Text.StdToken

checkUniqueHeads :: Gram -> Either Errors Gram
checkUniqueHeads g@(Gram ps) =
  if null multiples
    then pure g
    else throwError $
         Errors $
         S.fromList
           [ Error posn DuplicateHeadError' msg
           | (hd, posns) <- multiples
           , (posn, posns') <- chooseOne posns
           , let msg =
                   hsep
                     [ "Nonterminal"
                     , pretty $ show hd
                     , "is"
                     , "also"
                     , "defined"
                     , "at"
                     , sep (punctuate "," $ map pretty posns') <> "."
                     ]
           ]
  where
    multiples :: [(String, [Posn])]
    multiples = fmap (fmap sort) $ filter isMultiple $ collectOnFirst pairs
    isMultiple :: (s, [p]) -> Bool
    isMultiple = (> 1) . length . snd
    pairs :: [(String, Posn)]
    pairs = [(_tokenText hd, _tokenDeco hd) | Prod hd _ <- NE.toList ps]
