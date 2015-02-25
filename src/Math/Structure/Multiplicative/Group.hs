{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Math.Structure.Multiplicative.Group where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Data.Ord

import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative.Monoid


infixl 7 /
infixr 8 ^^

class MultiplicativeMonoid a => MultiplicativeGroup a where
  (/) :: a -> a -> a
  recip :: a -> a

  pow :: Integral n => n -> a -> a
  pow = powStd

  (^^) :: Integral n => a -> n -> a
  (^^) = flip pow


powStd :: (Integral n, MultiplicativeGroup a)
          => n -> a -> a
powStd n a =
  let na = pow0p (abs $ fromIntegral n) a
  in case compare n 0 of
       GT -> na
       EQ -> one
       LT -> recip na


deriving instance    MultiplicativeGroup a
                  => MultiplicativeGroup (NonZero a)
