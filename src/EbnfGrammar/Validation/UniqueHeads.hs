{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UniqueHeads
  ( checkUniqueHeads
  ) where

import Control.Monad.Except (MonadError)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import EbnfGrammar.Error
import EbnfGrammar.Posn
import EbnfGrammar.Syntax
import EbnfGrammar.Utils
import Prettyprinter
import Text.StdToken

checkUniqueHeads :: MonadError Errors m => Gram -> m Gram
checkUniqueHeads g@(Gram ps) =
  if null multiples
    then pure g
    else throwErrorList
           [ Error posn DuplicateHeadError msg
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
