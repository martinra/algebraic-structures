module Math.Structure.Multiplicative.Group where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Data.Ord

import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative.Magma
import Math.Structure.Multiplicative.Monoid


infixl 7 /
infixr 8 ^^

class MultiplicativeMonoid a => MultiplicativeGroup a where
  (/) :: a -> a -> a
  a / b = a * recip b

  recip :: a -> a
  recip a = one / a

  pow :: Integral n => n -> a -> a
  pow = powStd

  (^^) :: Integral n => a -> n -> a
  (^^) = flip pow


powStd :: (Integral n, MultiplicativeGroup a)
          => n -> a -> a
powStd n a =
  let na = pow0p (fromIntegral $ abs n) a
  in case compare n 0 of
       GT -> na
       EQ -> one
       LT -> recip na
