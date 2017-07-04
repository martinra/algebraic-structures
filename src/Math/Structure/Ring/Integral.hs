module Math.Structure.Ring.Integral where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Ring


class ( Ring r, Commutative r
      , MultiplicativeSemigroup (NonZero r) )
  => IntegralDomain r
