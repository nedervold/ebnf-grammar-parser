{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UniqueConstructors
  ( checkUniqueConstructors
  ) where

import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import EbnfGrammar.Error
import EbnfGrammar.Posn (Posn)
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (chooseOne, collectOnFirst)
import Prettyprinter
import Text.StdToken

checkUniqueConstructors :: Gram -> Either Errors Gram
checkUniqueConstructors g@(Gram ps) =
  if null multiples
    then pure g
    else throwErrorList
           [ Error posn DuplicateConstructorError msg
           | (ctor, posns) <- multiples
           , (posn, posns') <- chooseOne posns
           , let msg =
                   hsep
                     [ "Constructor"
                     , pretty $ show ctor
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
    pairs =
      [ (_tokenText ctor, _tokenDeco ctor)
      | Prod _hd alts <- NE.toList ps
      , Alt ctor _ <- NE.toList alts
      ]
