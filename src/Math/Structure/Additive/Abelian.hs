module Math.Structure.Additive.Abelian
where

import Prelude hiding ( (+), (-), negate, subtract )

import Math.Structure.Additive.Magma


class AdditiveMagma a => Abelian a
