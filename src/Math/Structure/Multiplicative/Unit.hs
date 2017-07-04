module Math.Structure.Multiplicative.Unit
where

import Data.Maybe

import Math.Structure.Multiplicative.Action
import Math.Structure.Multiplicative.Commutative
import Math.Structure.Multiplicative.DecidableOne
import Math.Structure.Multiplicative.Group
import Math.Structure.Multiplicative.Magma
import Math.Structure.Multiplicative.Monoid
import Math.Structure.Multiplicative.Semigroup


newtype Unit a = Unit { fromUnit :: a }
  deriving ( Show, Eq )


class MultiplicativeMonoid a => DecidableUnit a where
  isUnit :: a -> Bool
  toUnit :: a -> Unit a
  toUnitSafe :: a -> Maybe (Unit a)

  isUnit = isJust . toUnitSafe
  toUnit = fromJust . toUnitSafe
  toUnitSafe a = if isUnit a then Just (toUnit a) else Nothing


deriving instance MultiplicativeMagma a => MultiplicativeMagma (Unit a)

deriving instance Commutative a => Commutative (Unit a)

deriving instance MultiplicativeSemigroup a => MultiplicativeSemigroup (Unit a)

deriving instance MultiplicativeMonoid a => MultiplicativeMonoid (Unit a)

-- we do not derive further classes, which would yield wrong
-- constraints when plugged in by the typechecker
{-
deriving instance DecidableOne a => DecidableOne (Unit a)

deriving instance MultiplicativeGroup a => MultiplicativeGroup (Unit a)


instance MultiplicativeSemigroupLeftAction a s
  => MultiplicativeSemigroupLeftAction (Unit a) s
  where
    (Unit a) *. s = a *. s

instance MultiplicativeSemigroupRightAction a s
  => MultiplicativeSemigroupRightAction (Unit a) s
  where
    s .* (Unit a) = s .* a

instance MultiplicativeLeftAction a s => MultiplicativeLeftAction (Unit a) s

instance MultiplicativeRightAction a s => MultiplicativeRightAction (Unit a) s
-}
