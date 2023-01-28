{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Utils
  ( collectOnFirst
  , monotoneFixedPoint
  ) where

import Data.List (find, nub, sort)

collectOnFirst ::
     forall a b. Ord a
  => [(a, b)]
  -> [(a, [b])]
collectOnFirst abs' = [(a, [b' | (a', b') <- abs', a' == a]) | a <- as']
  where
    as' :: [a]
    as' = sort $ nub $ map fst abs'

monotoneFixedPoint ::
     forall a. Eq a
  => (a -> a)
  -> a
  -> a
monotoneFixedPoint f a = maybe err fst $ find (uncurry (==)) pairs
  where
    monotoneSeq :: [a]
    monotoneSeq = iterate f a
    pairs = zip monotoneSeq (tail monotoneSeq)
    err = error "error: monotoneFixedPoint never converged"
