module Math.Structure.Additive.Semigroup where

import qualified Prelude as P
import Prelude ( ($) )

import Data.List ( (!!), iterate )
import Numeric.Natural ( Natural(..) )

import Math.Structure.Additive.Magma


class AdditiveMagma a => AdditiveSemigroup a where
  -- todo: optimize
  sinnum1p :: Natural -> a -> a
  sinnum1p n a = (!! P.fromIntegral n) $ iterate (+a) a
