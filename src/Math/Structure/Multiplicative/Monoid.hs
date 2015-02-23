module Math.Structure.Multiplicative.Monoid where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Control.Arrow ( second )
import Numeric.Natural ( Natural(..) )

import Math.Structure.Multiplicative.Magma
import Math.Structure.Multiplicative.Semigroup


infixr 8 ^

class MultiplicativeSemigroup a => MultiplicativeMonoid a where
  one :: a

  pow0p :: Natural -> a -> a
  pow0p = pow0pStd

  (^) :: Integral n => a -> n -> a
  a^n = pow0p (fromIntegral n) a

pow0pStd :: MultiplicativeMonoid a
            => Natural -> a -> a
pow0pStd n a = (!! fromIntegral n) $ iterate (*a) one
