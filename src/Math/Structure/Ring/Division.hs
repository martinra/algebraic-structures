module Math.Structure.Ring.Division
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Integral


class (IntegralDomain r, MultiplicativeGroup (NonZero r)) => DivisionRing r
