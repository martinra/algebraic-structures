module Math.Structure.Multiplicative.Semigroup where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Numeric.Natural ( Natural(..) )

import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative.Magma


class MultiplicativeMagma a => MultiplicativeSemigroup a where
  pow1p :: Natural -> a -> a
  pow1p = pow1pStd

pow1pStd :: MultiplicativeSemigroup a
            => Natural -> a -> a
pow1pStd n a = (!! fromIntegral n) $ iterate (*a) a
