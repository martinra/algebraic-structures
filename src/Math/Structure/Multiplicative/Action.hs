{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Math.Structure.Multiplicative.Action
where

import Prelude hiding ( (*) )

import Math.Structure.Multiplicative.Monoid
import Math.Structure.Multiplicative.Semigroup


infixl 7 .*, *.

-- NOTE: we use reverse notation compared to Edward's algebra
-- package. The group has multiplication * and notation of action
-- should be defined in such a way that from the perspective of
-- the group, it remains the same as before. The . in *. should go
-- where the set being acted on ist.
class    MultiplicativeSemigroup g
      => MultiplicativeSemigroupLeftAction g s where
  (*.) :: g -> s -> s

class    MultiplicativeSemigroup g
      => MultiplicativeSemigroupRightAction g s where
  (.*) :: s -> g -> s


class    ( MultiplicativeMonoid g, MultiplicativeSemigroupLeftAction g s )
      => MultiplicativeLeftAction g s

class    ( MultiplicativeMonoid g, MultiplicativeSemigroupRightAction g s )
      => MultiplicativeRightAction g s
