module Math.Structure.Additive.Semigroup where

import Prelude hiding ( (+), (-), negate )

import Numeric.Natural ( Natural(..) )

import Math.Structure.Additive.Magma


class AdditiveMagma a => AdditiveSemigroup a where
  sinnum1p :: Natural -> a -> a
  sinnum1p = sinnum1pStd

sinnum1pStd :: AdditiveSemigroup a
            => Natural -> a -> a
sinnum1pStd n a = (!! fromIntegral n) $ iterate (+a) a
