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
  , isNonUnitalAlgebra
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

  , isSemiring
  , isRng
  , isRing
  , isEuclideanDomain
  , isField

  , intertwiningMorphisms
  , intertwiningInnerPairing
  , intertwiningPairing
  , intertwiningEndomorphisms
  , intertwiningBinaryOperators
  , intertwiningLeftActions
  , intertwiningRightActions

  , testPropertyQC
  , testPropertySnC
  , testPropertyQSC
  , testPropertyQSnC

  , runTestsQC
  , runTestsSnC
  , runTestsQSC
  , runTestsQSnC
  )
where

import Math.Structure.Tasty.Additive
import Math.Structure.Tasty.Intertwine
import Math.Structure.Tasty.Module
import Math.Structure.Tasty.Multiplicative
import Math.Structure.Tasty.NonZero
import Math.Structure.Tasty.Ring

import Math.Structure.Utility.Tasty

