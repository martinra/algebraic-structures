{-# LANGUAGE 
    FlexibleInstances
  #-}

module Math.Structure.Ring.Ring where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Rig
import Math.Structure.Ring.Rng


class (Rig r, Rng r) => Ring r
