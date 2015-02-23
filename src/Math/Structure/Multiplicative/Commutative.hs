module Math.Structure.Multiplicative.Commutative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Math.Structure.Multiplicative.Magma


class MultiplicativeMagma a => Commutative a
