{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  #-}

module Math.Structure.Tasty.Module
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy
import Test.Tasty

import Math.Structure.Module
import Math.Structure.Ring
import Math.Structure.Utils.Tasty

import Math.Structure.Tasty.Additive
import Math.Structure.Tasty.Multiplicative
import Math.Structure.Tasty.Ring


isLeftModule ::
     ( Testable r, Testable m, LeftModule r m )
  => Proxy r -> Proxy m -> [TestTree]
isLeftModule pr pm =
  isAbeleanGroup pm ++
  isMultiplicativeLeftAction pr pm ++
  [ isLinearSemiringLeftAction' pr pm ]

isRightModule ::
     ( Testable r, Testable m, RightModule r m )
  => Proxy r -> Proxy m -> [TestTree]
isRightModule pr pm =
  isAbeleanGroup pm ++
  isMultiplicativeRightAction pr pm ++
  [ isLinearSemiringRightAction' pr pm ]

isModule ::
     ( Testable r, Testable m, Module r m )
  => Proxy r -> Proxy m -> [TestTree]
isModule pr pm =
  isAbeleanGroup pm ++
  isMultiplicativeLeftAction pr pm ++
  isMultiplicativeRightAction pr pm ++
  [ isLinearSemiringLeftAction' pr pm
  , isLinearSemiringRightAction' pr pm
  , isModule' pr pm
  ]

isLinearSemiringLeftAction' :: forall r m .
     ( Testable r, Testable m
     , Semiring r, AdditiveGroup m
     , LinearSemiringLeftAction r m )
  => Proxy r -> Proxy m -> TestTree
isLinearSemiringLeftAction' pr pm =
  testProperty "Linear semiring left action" $
    \r r' m -> ((r::r) + (r'::r)) *. (m::m) == r *. m + r' *. m

isLinearSemiringRightAction' :: forall r m .
     ( Testable r, Testable m
     , Semiring r, AdditiveGroup m
     , LinearSemiringRightAction r m )
  => Proxy r -> Proxy m -> TestTree
isLinearSemiringRightAction' pr pm =
  testProperty "Linear semiring left action" $
    \r r' m -> (m::m) .* ((r::r) + (r'::r)) == m .* r + m .* r'

isModule' :: forall r m .
     ( Testable r, Testable m
     , Module r m )
  => Proxy r -> Proxy m -> TestTree
isModule' pr pm = 
  testProperty "Module" $
    \r m -> (r::r) *. (m::m) == m .* r
