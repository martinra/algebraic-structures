module Math.Structure.Multiplicative.StdInstances
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P

import Math.Structure.Multiplicative.Commutative
import Math.Structure.Multiplicative.Magma
import Math.Structure.Multiplicative.Group
import Math.Structure.Multiplicative.Monoid
import Math.Structure.Multiplicative.Semigroup


instance MultiplicativeMagma Integer where
  (*) = (P.*)
instance Commutative Integer
instance MultiplicativeSemigroup Integer
instance MultiplicativeMonoid Integer where
  one = 1
