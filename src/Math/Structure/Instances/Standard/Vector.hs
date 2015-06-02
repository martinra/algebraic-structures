{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Math.Structure.Instances.Standard.Vector
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Numeric.Natural ( Natural )
 
import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Module
import Math.Structure.Multiplicative
import Math.Structure.Ring


instance AdditiveMagma a => AdditiveMagma (Vector a) where
  (+) = V.zipWith (+)

instance AdditiveSemigroup a => AdditiveSemigroup (Vector a) where
  sinnum1p n = V.map (sinnum1p n)

instance Abelian a => Abelian (Vector a)


instance Rng a => MultiplicativeSemigroupLeftAction a (Vector a) where
  a *. v = V.map (a*) v

instance Rng a => MultiplicativeSemigroupRightAction a (Vector a) where
  v .* a = V.map (*a) v

instance ( Abelian a, Rng a ) => LinearSemiringLeftAction a (Vector a)
instance ( Abelian a, Rng a ) => LinearSemiringRightAction a (Vector a)
