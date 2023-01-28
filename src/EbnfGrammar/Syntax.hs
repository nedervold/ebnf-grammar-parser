{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EbnfGrammar.Syntax
  ( Gram(..)
  , Prod(..)
  , Alt(..)
  , Term(..)
  , Vocab(..)
  , PA(..)
  ) where

import Data.Data (Data)
import Data.Function (on)
import Data.Generics.Uniplate.Data ()
import Data.Ord (comparing)
import EbnfGrammar.Token (Token)
import Language.Ebnf.Extensions.Syntax (Rep1)
import Text.StdToken (_tokenText)

{- Note that this is abstract syntax, not concrete. -}
newtype Gram =
  Gram (Rep1 Prod)
  deriving (Data, Eq, Show)

data Prod =
  Prod Token (Rep1 Alt)
  deriving (Data, Eq, Show)

data Alt =
  Alt Token [Term]
  deriving (Data, Eq, Show)

------------------------------
data PA
  = P Prod
  | A Alt
  deriving (Eq, Show)

instance Ord PA
  -- assume no duplicate heads, no duplicate constructors
                                                          where
  compare (P (Prod hd _)) (P (Prod hd' _)) = comparing _tokenText hd hd'
  compare (A (Alt ctor _)) (A (Alt ctor' _)) = comparing _tokenText ctor ctor'
  compare (P _) (A (Alt _ _)) = LT
  compare (A (Alt _ _)) (P _) = GT

------------------------------
data Term
  = VocabTerm Vocab
  | Opt Vocab
  | Rep0 Vocab
  | Rep1 Vocab
  | Repsep0 Vocab Vocab
  | Repsep1 Vocab Vocab
  deriving (Data, Eq, Show)

data Vocab
  = NT Token
  | T Token
  deriving (Data, Eq, Show)

instance Ord Vocab where
  compare (NT _) (T _) = LT
  compare (NT tok) (NT tok') = (compare `on` _tokenText) tok tok'
  compare (T _) (NT _) = GT
  compare (T tok) (T tok') = (compare `on` _tokenText) tok tok'
