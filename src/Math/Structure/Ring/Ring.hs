{-# LANGUAGE 
    FlexibleInstances
  #-}

module Math.Structure.Ring.Ring where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Ring.Rig
import Math.Structure.Ring.Rng


class (Rig r, Rng r) => Ring r

-- This is not always the case, because one == zero cannot be excluded by type restrictions
-- deriving instance Ring r => MultiplicativeMonoid (NonZero r)
