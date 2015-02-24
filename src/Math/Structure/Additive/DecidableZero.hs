module Math.Structure.Additive.DecidableZero
where

import Prelude hiding ( (+), (-), negate, subtract )

import Math.Structure.Additive.Monoid


class AdditiveMonoid a => DecidableZero a where
  isZero :: a -> Bool
