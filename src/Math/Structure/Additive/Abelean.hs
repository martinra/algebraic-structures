module Math.Structure.Additive.Abelean
where

import Prelude hiding ( (+), (-), negate )

import Math.Structure.Additive.Magma


class AdditiveMagma a => Abelean a
