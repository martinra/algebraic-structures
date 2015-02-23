module Math.Structure.Additive.Magma
where

import Prelude hiding ( (+), (-), negate, subtract )


infixl 6 +

class AdditiveMagma a where
  (+) :: a -> a -> a
