module Math.Structure.Ring.PID
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd, xgcd
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Integral

-- we do not assume Factorial r,
-- because this is typical a lot of extra effort to implement
class IntegralDomain r => PIDomain r where
  gcd :: r -> r -> r
  xgcd :: r -> r -> (r,r,r)
