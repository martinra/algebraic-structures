module Math.Structure.Additive.Monoid where

import Prelude hiding ( (+), (-), negate, subtract )

import Numeric.Natural ( Natural(..) )

import Math.Structure.Additive.Magma
import Math.Structure.Additive.Semigroup


class AdditiveSemigroup a => AdditiveMonoid a where
  zero :: a

  sinnum0p :: Natural -> a -> a
  sinnum0p = sinnum0pStd

sinnum0pStd :: AdditiveMonoid a
            => Natural -> a -> a
sinnum0pStd n a = (!! fromIntegral n) $ iterate (+a) zero
