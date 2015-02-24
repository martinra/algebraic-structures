module Math.Structure.Multiplicative.DecidableOne
where

import Prelude hiding ( (*), (/), recip )

import Math.Structure.Multiplicative.Monoid


class MultiplicativeMonoid a => DecidableOne a where
  isOne :: a -> Bool
