module Math.Structure.Ring.Distributive
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative


class (AdditiveMagma r, MultiplicativeMagma r) => Distributive r
