{-# LANGUAGE 
    FlexibleInstances
  #-}

module Math.Structure.Ring.Division
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Integral
import Math.Structure.Ring.Ring


class (Ring r, MultiplicativeGroup r) => DivisionRing r
