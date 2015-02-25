{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Math.Structure.Multiplicative.Commutative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative.Magma


class MultiplicativeMagma a => Commutative a

deriving instance    Commutative a
                  => Commutative (NonZero a)
