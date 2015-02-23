module Math.Structure.Additive.Group where

import Prelude hiding ( (+), (-), negate )

import Data.Ord

import Math.Structure.Additive.Monoid


class AdditiveMonoid a => AdditiveGroup a where
  (-) :: a -> a -> a
  negate :: a -> a

  sinnum :: Integral n => n -> a -> a
  sinnum = sinnumStd

sinnumStd :: (Integral n, AdditiveGroup a)
          => n -> a -> a
sinnumStd n a =
  let na = sinnum0p (abs $ fromIntegral n) a
  in case compare n 0 of
       GT -> na
       EQ -> zero
       LT -> negate na
