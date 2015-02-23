module Math.Structure.Additive.Abelean
where

import Prelude hiding ( (+), (-), negate, subtract )

import Math.Structure.Additive.Magma


class AdditiveMagma a => Abelean a
