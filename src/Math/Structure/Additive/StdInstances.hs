module Math.Structure.Additive.StdInstances
where

import Prelude hiding ( (+), (-), negate, subtract )
import qualified Prelude as P

import Math.Structure.Additive.Abelean
import Math.Structure.Additive.Magma
import Math.Structure.Additive.Group
import Math.Structure.Additive.Monoid
import Math.Structure.Additive.Semigroup


instance AdditiveMagma Integer where
  (+) = (P.+)
instance Abelean Integer
instance AdditiveSemigroup Integer
instance AdditiveMonoid Integer where
  zero = 0
instance AdditiveGroup Integer where
  (-) = (P.-)
  negate = P.negate
  subtract = P.subtract
  
