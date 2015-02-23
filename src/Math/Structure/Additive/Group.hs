module Math.Structure.Additive.Group where

import qualified Prelude as P
import Prelude ( ($)
               , Integral(..)
               )
import Data.Ord

import Math.Structure.Additive.Monoid


class AdditiveMonoid a => AdditiveGroup a where
  (-) :: a -> a -> a
  negate :: a -> a

  sinnum :: Integral n => n -> a -> a
  sinnum n a = case compare n 0 of
    GT -> sinnum0p (P.fromIntegral n) a 
    EQ -> zero
    LT -> negate $ sinnum0p (P.negate $ P.fromIntegral n) a
