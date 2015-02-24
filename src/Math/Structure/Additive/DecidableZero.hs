module Math.Structure.Additive.DecidableZero
where

import Prelude hiding ( (+), (-), negate, subtract )

import Math.Structure.Additive.Monoid


class AdditiveMonoid a => DecidableZero a where
  isZero :: a -> Bool


newtype NonZero a = NonZero { fromNonZero :: a }
  deriving (Show, Eq)

nonZero :: DecidableZero a => a -> NonZero a
nonZero a | isZero a = error "NonZero 0 in Math.Structure.Additive.DecidableZero"
          | otherwise = NonZero a
