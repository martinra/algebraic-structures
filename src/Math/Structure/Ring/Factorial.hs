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
import Math.Structure.Ring.Ring


data Factored a b = Factored b (Vector (a,Natural))

class Ring r => FactorialRing r where
  factor :: r -> Factored r r
