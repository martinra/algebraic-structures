{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Math.Structure.Module.LinearAction
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Semiring


class    ( Semiring r, AdditiveSemigroup m, Abelian m
         , MultiplicativeSemigroupLeftAction r m )
      => LinearSemiringLeftAction r m 

class    ( Semiring r, AdditiveSemigroup m, Abelian m
         , MultiplicativeSemigroupRightAction r m )
      => LinearSemiringRightAction r m
