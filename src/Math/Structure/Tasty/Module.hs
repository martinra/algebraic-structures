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
  => Proxy r -> Proxy m -> TestR [TestTree]
isLeftModule pr pm = fmap concat $ sequence
  [ isAbelianGroup pm
  , sequence
    [ isLeftModule' pr pm ]
  ]

isRightModule ::
     ( Testable r, Testable m, RightModule r m )
  => Proxy r -> Proxy m -> TestR [TestTree]
isRightModule pr pm = fmap concat $ sequence
  [ isAbelianGroup pm
  , sequence
    [ isRightModule' pr pm ]
  ]

isModule ::
     ( Testable r, Testable m, Module r m )
  => Proxy r -> Proxy m -> TestR [TestTree]
isModule pr pm = fmap concat $ sequence
  [ isAbelianGroup pm
  , sequence
    [ isLeftModule' pr pm
    , isRightModule' pr pm
    , isModule' pr pm
    ]
  ]

isLeftAlgebra ::
     ( Testable r, Testable a, LeftAlgebra r a )
  => Proxy r -> Proxy a -> TestR [TestTree]
isLeftAlgebra pr pa = fmap concat $ sequence
  [ isRing pa
  , sequence
    [ isLeftModule' pr pa
    , isLeftAlgebra' pr pa
    ]
  ]

isRightAlgebra ::
     ( Testable r, Testable a, RightAlgebra r a )
  => Proxy r -> Proxy a -> TestR [TestTree]
isRightAlgebra pr pa = fmap concat $ sequence
  [ isRing pa
  , sequence
    [ isRightModule' pr pa
    , isRightAlgebra' pr pa
    ]
  ]

isNonUnitalAlgebra ::
     ( Testable r, Testable a, Algebra r a )
  => Proxy r -> Proxy a -> TestR [TestTree]
isNonUnitalAlgebra pr pa = fmap concat $ sequence
  [ isRng pa
  , sequence
    [ isLeftModule' pr pa
    , isRightModule' pr pa
    , isModule' pr pa
    , isLeftAlgebra' pr pa
    , isRightAlgebra' pr pa
    ]
  ]

isAlgebra ::
     ( Testable r, Testable a, Algebra r a )
  => Proxy r -> Proxy a -> TestR [TestTree]
isAlgebra pr pa = fmap concat $ sequence
  [ isNonUnitalAlgebra pr pa
  , sequence
    [ isMultiplicativeMonoid' pa ]
  ]


isLinearSemiringLeftAction' :: forall r m .
     ( Testable r, Testable m
     , LinearSemiringLeftAction r m )
  => Proxy r -> Proxy m -> TestR TestTree
isLinearSemiringLeftAction' pr pm = withTestProperty $ \testProperty ->
  testProperty "Linear semiring left action" $
    \r r' m -> ((r::r) + (r'::r)) *. (m::m) == r *. m + r' *. m

isLinearSemiringRightAction' :: forall r m .
     ( Testable r, Testable m
     , LinearSemiringRightAction r m )
  => Proxy r -> Proxy m -> TestR TestTree
isLinearSemiringRightAction' pr pm = withTestProperty $ \testProperty ->
  testProperty "Linear semiring left action" $
    \r r' m -> (m::m) .* ((r::r) + (r'::r)) == m .* r + m .* r'

isLeftModule' ::
     ( Testable r, Testable m, LeftModule r m )
  => Proxy r -> Proxy m -> TestR TestTree
isLeftModule' pr pm = 
  fmap (testGroup "Left module") $
  fmap concat $ sequence
  [ isMultiplicativeLeftAction pr pm
  , sequence
    [ isLinearSemiringLeftAction' pr pm ]
  ]

isRightModule' ::
     ( Testable r, Testable m, RightModule r m )
  => Proxy r -> Proxy m -> TestR TestTree
isRightModule' pr pm = 
  fmap (testGroup "Right module") $
  fmap concat $ sequence
  [ isMultiplicativeRightAction pr pm
  , sequence
    [ isLinearSemiringRightAction' pr pm ]
  ]

isModule' :: forall r m .
     ( Testable r, Testable m, Module r m )
  => Proxy r -> Proxy m -> TestR TestTree
isModule' pr pm = withTestProperty $ \testProperty ->
  testProperty "Module" $
    \r m -> (r::r) *. (m::m) == m .* r

isLeftAlgebra' :: forall r a .
     ( Testable r, Testable a, LeftAlgebra r a )
  => Proxy r -> Proxy a -> TestR TestTree
isLeftAlgebra' pr pa = withTestProperty $ \testProperty ->
  testProperty "Left algebra" $
    \r a a' -> (r::r) *. ((a::a) * (a'::a)) == ( r *. a) * a'

isRightAlgebra' :: forall r a .
     ( Testable r, Testable a, RightAlgebra r a )
  => Proxy r -> Proxy a -> TestR TestTree
isRightAlgebra' pr pa = withTestProperty $ \testProperty ->
  testProperty "Right algebra" $
    \r a a' -> ((a::a) * (a'::a)) .* (r::r) == a * (a' .* r)
