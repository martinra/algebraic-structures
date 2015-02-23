module Math.Structure.Ring.PID
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd, xgcd
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Ring

class Ring r => PIDRing r where
  gcd :: r -> r -> r
  xgcd :: r -> r -> (r,r,r)
