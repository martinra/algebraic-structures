{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Math.Structure.Module.Module
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Module.LinearAction
import Math.Structure.Ring.Semiring


class    ( Semiring r, AbeleanGroup m
         , MultiplicativeLeftAction r m
         , LinearSemiringLeftAction r m )
      => LeftModule r m

class    ( Semiring r, AbeleanGroup m
         , MultiplicativeRightAction r m
         , LinearSemiringRightAction r m )
      => RightModule r m
