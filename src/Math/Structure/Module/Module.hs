{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Math.Structure.Module.Module
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Module.LinearAction
import Math.Structure.Ring.Semiring
import Math.Structure.Ring.Rng
import Math.Structure.Ring.Ring


class    ( Semiring r, AbelianGroup m
         , MultiplicativeLeftAction r m
         , LinearSemiringLeftAction r m )
      => LeftModule r m

class    ( Semiring r, AbelianGroup m
         , MultiplicativeRightAction r m
         , LinearSemiringRightAction r m )
      => RightModule r m

class    ( Commutative r
         , LeftModule r m, RightModule r m )
      => Module r m

instance Ring r => LeftModule r r
instance Ring r => RightModule r r
instance ( Commutative r, Ring r ) => Module r r


class    ( Semiring r, AbelianGroup m
         , MultiplicativeSemigroupLeftAction r m
         , LinearSemiringLeftAction r m )
      => NonUnitalLeftModule r m

class    ( Semiring r, AbelianGroup m
         , MultiplicativeSemigroupRightAction r m
         , LinearSemiringRightAction r m )
      => NonUnitalRightModule r m

class    ( Commutative r
         , NonUnitalLeftModule r m, NonUnitalRightModule r m )
      => NonUnitalModule r m

instance Rng r => NonUnitalLeftModule r r
instance Rng r => NonUnitalRightModule r r
instance ( Commutative r, Rng r ) => NonUnitalModule r r
