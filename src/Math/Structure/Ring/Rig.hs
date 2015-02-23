{-# LANGUAGE 
    FlexibleInstances
  #-}

module Math.Structure.Ring.Rig where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Semiring


class (Semiring r, MultiplicativeMonoid r) => Rig r
