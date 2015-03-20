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
  [ isLeftModule' pr pm ]

isRightModule ::
     ( Testable r, Testable m, RightModule r m )
  => Proxy r -> Proxy m -> [TestTree]
isRightModule pr pm =
  isAbeleanGroup pm ++
  [ isRightModule' pr pm ]

isModule ::
     ( Testable r, Testable m, Module r m )
  => Proxy r -> Proxy m -> [TestTree]
isModule pr pm =
  isAbeleanGroup pm ++
  [ isLeftModule' pr pm
  , isRightModule' pr pm
  , isModule' pr pm
  ]

isLeftAlgebra ::
     ( Testable r, Testable a, LeftAlgebra r a )
  => Proxy r -> Proxy a -> [TestTree]
isLeftAlgebra pr pa =
  isRing pa ++
  [ isLeftModule' pr pa
  , isLeftAlgebra' pr pa ]

isRightAlgebra ::
     ( Testable r, Testable a, RightAlgebra r a )
  => Proxy r -> Proxy a -> [TestTree]
isRightAlgebra pr pa =
  isRing pa ++
  [ isRightModule' pr pa
  , isRightAlgebra' pr pa ]

isAlgebra ::
     ( Testable r, Testable a, Algebra r a )
  => Proxy r -> Proxy a -> [TestTree]
isAlgebra pr pa =
  isRing pa ++
  [ isLeftModule' pr pa
  , isRightModule' pr pa
  , isModule' pr pa
  , isLeftAlgebra' pr pa
  , isRightAlgebra' pr pa
  ]


isLinearSemiringLeftAction' :: forall r m .
     ( Testable r, Testable m
     , LinearSemiringLeftAction r m )
  => Proxy r -> Proxy m -> TestTree
isLinearSemiringLeftAction' pr pm =
  testProperty "Linear semiring left action" $
    \r r' m -> ((r::r) + (r'::r)) *. (m::m) == r *. m + r' *. m

isLinearSemiringRightAction' :: forall r m .
     ( Testable r, Testable m
     , LinearSemiringRightAction r m )
  => Proxy r -> Proxy m -> TestTree
isLinearSemiringRightAction' pr pm =
  testProperty "Linear semiring left action" $
    \r r' m -> (m::m) .* ((r::r) + (r'::r)) == m .* r + m .* r'

isLeftModule' ::
     ( Testable r, Testable m, LeftModule r m )
  => Proxy r -> Proxy m -> TestTree
isLeftModule' pr pm =
  testGroup "Left module" $
    isMultiplicativeLeftAction pr pm ++
    [ isLinearSemiringLeftAction' pr pm ]

isRightModule' ::
     ( Testable r, Testable m, RightModule r m )
  => Proxy r -> Proxy m -> TestTree
isRightModule' pr pm =
  testGroup "Right module" $
    isMultiplicativeRightAction pr pm ++
    [ isLinearSemiringRightAction' pr pm ]

isModule' :: forall r m .
     ( Testable r, Testable m, Module r m )
  => Proxy r -> Proxy m -> TestTree
isModule' pr pm = 
  testProperty "Module" $
    \r m -> (r::r) *. (m::m) == m .* r

isLeftAlgebra' :: forall r a .
     ( Testable r, Testable a, LeftAlgebra r a )
  => Proxy r -> Proxy a -> TestTree
isLeftAlgebra' pr pa =
  testProperty "Left algebra" $
    \r a a' -> (r::r) *. ((a::a) * (a'::a)) == ( r *. a) * a

isRightAlgebra' :: forall r a .
     ( Testable r, Testable a, RightAlgebra r a )
  => Proxy r -> Proxy a -> TestTree
isRightAlgebra' pr pa =
  testProperty "Right algebra" $
    \r a a' -> ((a::a) * (a'::a)) .* (r::r) == a * (a' .* r)
