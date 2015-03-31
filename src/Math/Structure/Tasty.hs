module Math.Structure.Tasty
  ( runTestR

  , isAdditiveSemigroup
  , isAbelianSemigroup
  , isAdditiveGroup
  , isAbelianGroup
  , hasDecidableZero

  , isLeftModule
  , isRightModule
  , isModule
  , isLeftAlgebra
  , isRightAlgebra
  , isAlgebra

  , isMultiplicativeSemigroup
  , isCommutativeSemigroup
  , isMultiplicativeMonoid
  , isCommutativeMonoid
  , isMultiplicativeGroup
  , isCommutativeGroup
  , hasDecidableOne
  , isMultiplicativeLeftAction
  , isMultiplicativeRightAction

  , isRing
  , isEuclideanDomain
  , isField
  )
where

import Math.Structure.Tasty.Additive
import Math.Structure.Tasty.Module
import Math.Structure.Tasty.Multiplicative
import Math.Structure.Tasty.NonZero
import Math.Structure.Tasty.Ring

import Math.Structure.Utils.Tasty
