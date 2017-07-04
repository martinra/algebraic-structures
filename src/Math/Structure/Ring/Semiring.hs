module Math.Structure.Ring.Semiring where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Distributive


class ( AdditiveSemigroup r, MultiplicativeSemigroup r
      , Abelian r, Distributive r ) => Semiring r
