module Math.Structure.Additive.Group where

import Prelude hiding ( (+), (-), negate, subtract )

import Data.Ord

import Math.Structure.Additive.Magma
import Math.Structure.Additive.Monoid


infixl 6 -

class AdditiveMonoid a => AdditiveGroup a where
  (-) :: a -> a -> a
  a - b = a + negate b

  negate :: a -> a
  negate a = zero - a

  subtract :: a -> a -> a
  subtract = flip (-)

  sinnum :: Integral n => n -> a -> a
  sinnum = sinnumStd

sinnumStd :: (Integral n, AdditiveGroup a)
          => n -> a -> a
sinnumStd n a =
  let na = sinnum0p (fromIntegral $ abs n) a
  in case compare n 0 of
       GT -> na
       EQ -> zero
       LT -> negate na
