{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Math.Structure.Module.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Module.Module
import Math.Structure.Module.LinearAction
import Math.Structure.Multiplicative
import Math.Structure.Ring


class ( Semiring a, LeftModule r a ) => SemiLeftAlgebra r a
class ( Rng a, LeftModule r a ) => NonUnitalLeftAlgebra r a
class ( Ring a, LeftModule r a ) => LeftAlgebra r a

class ( Semiring a, RightModule r a ) => SemiRightAlgebra r a
class ( Rng a, RightModule r a ) => NonUnitalRightAlgebra r a
class ( Ring a, RightModule r a ) => RightAlgebra r a

class    ( Commutative r, SemiLeftAlgebra r a, SemiRightAlgebra r a )
      => SemiAlgebra r a

class    ( Commutative r
         , NonUnitalLeftAlgebra r a, NonUnitalRightAlgebra r a
         , Module r a )
      => NonUnitalAlgebra r a

class    ( Commutative r
         , LeftAlgebra r a, RightAlgebra r a
         , Module r a )
      => Algebra r a
