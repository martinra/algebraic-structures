{-# LANGUAGE
    FlexibleContexts
  #-}

module Math.Structure.Ring.Field
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Division


class (DivisionRing r, Commutative r, Commutative (NonZero r)) => Field r
