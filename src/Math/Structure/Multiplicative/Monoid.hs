module Math.Structure.Multiplicative.Monoid where

import Prelude hiding ( (*), (/), recip )

import Numeric.Natural ( Natural(..) )

import Math.Structure.Multiplicative.Magma
import Math.Structure.Multiplicative.Semigroup


class MultiplicativeSemigroup a => MultiplicativeMonoid a where
  one :: a

  pow0p :: Natural -> a -> a
  pow0p = pow0pStd

pow0pStd :: MultiplicativeMonoid a
            => Natural -> a -> a
pow0pStd n a = (!! fromIntegral n) $ iterate (*a) one
