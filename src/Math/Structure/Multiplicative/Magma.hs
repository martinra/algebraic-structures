module Math.Structure.Multiplicative.Magma
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )


infixl 7 *

class MultiplicativeMagma a where
  (*) :: a -> a -> a
