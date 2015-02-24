module Math.Structure.Module.Module
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive

class (Semiring r, AdditiveGroup m) => LeftModule r m where
  (.*) :: r -> m -> m
