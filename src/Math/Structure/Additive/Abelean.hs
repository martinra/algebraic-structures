{-# LANGUAGE
    ConstraintKinds
  #-}

module Math.Structure.Additive.Abelean
where

import Prelude hiding ( (+), (-), negate, subtract )

import Math.Structure.Additive.Magma
import Math.Structure.Additive.Group


class AdditiveMagma a => Abelean a

type AbeleanGroup a = (AdditiveGroup a, Abelean a)
