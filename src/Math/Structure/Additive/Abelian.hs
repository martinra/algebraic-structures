module Math.Structure.Additive.Abelian
where

import Prelude hiding ( (+), (-), negate, subtract )

import Math.Structure.Additive.Magma
import Math.Structure.Additive.Group


class AdditiveMagma a => Abelian a

type AbelianGroup a = (AdditiveGroup a, Abelian a)
