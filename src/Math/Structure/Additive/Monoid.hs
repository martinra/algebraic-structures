module Math.Structure.Additive.Monoid where

import qualified Prelude as P
import Prelude ( ($) )

import Data.List ( (!!), iterate )
import Numeric.Natural ( Natural(..) )

import Math.Structure.Additive.Magma
import Math.Structure.Additive.Semigroup


class Semigroup a => Monoid a where
  zero :: a

  -- todo: optimize
  sinnum0p :: Natural -> a -> a
  sinnum0p n a = (!! P.fromIntegral n) $ iterate (+a) zero
