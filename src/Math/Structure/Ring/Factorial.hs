module Math.Structure.Ring.Factorial
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Numeric.Natural ( Natural )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Integral


data Factored r = Factored (Unit r) (Vector (r,Natural))

class IntegralDomain r => FactorialRing r where
  factor :: r -> Factored r
