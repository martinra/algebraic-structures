module Math.Structure.Additive.Magma
where

import qualified Prelude as P


infixl 6 +

class Magma a where
  (+) :: a -> a -> a
