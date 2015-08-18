module Math.Structure.Ring.Rng where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Ring.Semiring


class (Semiring r, AdditiveGroup r) => Rng r
