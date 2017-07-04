module Math.Structure.Multiplicative.Commutative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative.Group
import Math.Structure.Multiplicative.Magma


class MultiplicativeMagma a => Commutative a

type CommutativeGroup a = ( MultiplicativeGroup a, Commutative a )
