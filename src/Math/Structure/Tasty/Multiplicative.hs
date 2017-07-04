{-# LANGUAGE
    FlexibleContexts
  , ConstraintKinds
  , ScopedTypeVariables
  #-}

module Math.Structure.Tasty.Multiplicative where

import Prelude hiding ( (*), (/), recip, (^), (^^) )

import Data.Proxy
import Numeric.Natural ( Natural(..) )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Natural
import Test.QuickCheck.Arbitrary ()

import Math.Structure.Multiplicative
-- import Math.Structure.Multiplicative.Semigroup ( pow1pStd )
-- import Math.Structure.Multiplicative.Monoid ( pow0pStd )
-- import Math.Structure.Multiplicative.Group ( powStd )
import Math.Structure.Utility.Tasty


isMultiplicativeSemigroup ::
     ( Testable a, MultiplicativeSemigroup a )
  => Proxy a -> TestR [TestTree]
isMultiplicativeSemigroup p = sequence
  [ isMultiplicativeSemigroup' p ]

isCommutativeSemigroup ::
     ( Testable a, Commutative a, MultiplicativeSemigroup a )
  => Proxy a -> TestR [TestTree]
isCommutativeSemigroup p = sequence
  [ isCommutative' p
  , isMultiplicativeSemigroup' p
  ]

isMultiplicativeMonoid ::
     ( Testable a, MultiplicativeMonoid a )
  => Proxy a -> TestR [TestTree]
isMultiplicativeMonoid p = sequence
  [ isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  ]

isCommutativeMonoid ::
     ( Testable a, Commutative a, MultiplicativeMonoid a )
  => Proxy a -> TestR [TestTree]
isCommutativeMonoid p = sequence
  [ isCommutative' p
  , isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  ]

isMultiplicativeGroup ::
     ( Testable a, MultiplicativeGroup a )
  => Proxy a -> TestR [TestTree]
isMultiplicativeGroup p = sequence
  [ isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  , isMultiplicativeGroup' p
  ]

isCommutativeGroup ::
    ( Testable a, CommutativeGroup a )
  => Proxy a -> TestR [TestTree]
isCommutativeGroup p = sequence
  [ isCommutative' p
  , isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  , isMultiplicativeGroup' p
  ]

hasDecidableOne :: forall a.
     ( Testable a, DecidableOne a )
  => Proxy a -> TestR [TestTree]
hasDecidableOne p = withTestProperty $ \testProperty ->
  [ testGroup "Decidable One"
    [ testCase "isOne one" $ isOne (one::a) @?= True
    , testProperty "isOne <=> (==one)" $
        \a -> ((a::a) == one) == isOne a
    ]
  ]


isMultiplicativeSemigroupLeftAction :: forall g s.
     ( Testable g, Testable s, MultiplicativeSemigroupLeftAction g s )
  => Proxy g -> Proxy s -> TestR [TestTree]
isMultiplicativeSemigroupLeftAction pg ps =
  withTestProperty $ \testProperty ->
  [ testGroup "Multiplicative Semigroup Left Action"
    [ testProperty "Semigroup action" $
        \g h s -> ((g::g) * (h::g)) *. (s::s) == g *. (h *. s)
    ]
  ]

isMultiplicativeLeftAction :: forall g s.
     ( Testable g, Testable s, MultiplicativeLeftAction g s )
  => Proxy g -> Proxy s -> TestR [TestTree]
isMultiplicativeLeftAction pg ps = withTestProperty $ \testProperty ->
  [ testGroup "Multiplicative Left Action"
    [ testProperty "Semigroup action" $
        \g h s -> ((g::g) * (h::g)) *. (s::s) == g *. (h *. s)
    , testProperty "Monoid action" $
        \s -> (one::g) *. (s::s) == s
    ]
  ]

isMultiplicativeSemigroupRightAction :: forall g s.
     ( Testable g, Testable s, MultiplicativeSemigroupRightAction g s )
  => Proxy g -> Proxy s -> TestR [TestTree]
isMultiplicativeSemigroupRightAction pg ps =
  withTestProperty $ \testProperty ->
  [ testGroup "Multiplicative Semigroup Right Action"
    [ testProperty "Semigroup action" $
        \g h s -> (s::s) .* ((g::g) * (h::g)) == (s .* g) .* h
    ]
  ]

isMultiplicativeRightAction :: forall g s.
     ( Testable g, Testable s, MultiplicativeRightAction g s )
  => Proxy g -> Proxy s -> TestR [TestTree]
isMultiplicativeRightAction pg ps = withTestProperty $ \testProperty ->
  [ testGroup "Multiplicative Right Action"
    [ testProperty "Semigroup action" $
        \g h s -> (s::s) .* ((g::g) * (h::g)) == (s .* g) .* h
    , testProperty "Monoid action" $
        \s -> (s::s) .* (one::g) == s
    ]
  ]


isCommutative' :: forall a .
     ( Testable a, Commutative a )
  => Proxy a -> TestR TestTree
isCommutative' p = withTestProperty $ \testProperty ->
  testProperty "Multiplicative Commutative" $
                 \a b -> (a::a) * (b::a) == b * a

isMultiplicativeSemigroup' :: forall a .
     ( Testable a, MultiplicativeSemigroup a )
  => Proxy a -> TestR TestTree
isMultiplicativeSemigroup' p = withTestProperty $ \testProperty ->
  testGroup "Multiplicative Semigroup"
  [ testProperty "associative" $
      \a b c -> (a::a) * ((b::a) * (c::a)) == (a * b) * c
--  , testProperty "pow1p" $
--      \n a ->  pow1p n (a::a) == pow1pStd n a
  ]

isMultiplicativeMonoid' :: forall a .
     ( Testable a, MultiplicativeMonoid a )
  => Proxy a -> TestR TestTree
isMultiplicativeMonoid' p = withTestProperty $ \testProperty ->
  testGroup "Multiplicative Monoid"
  [ testProperty "one" $
      \a -> (one::a) * (a::a) == a && a * (one::a) == a
--  , testProperty "pow0p" $
--      \n a ->  pow0p n (a::a) == pow0pStd n a
--  , testProperty "(^)" $
--      \n a ->  (a::a)^(toInteger (n :: Natural)) == pow0pStd n a
  ]

isMultiplicativeGroup' :: forall a .
     ( Testable a, MultiplicativeGroup a )
  => Proxy a -> TestR TestTree
isMultiplicativeGroup' p = withTestProperty $ \testProperty ->
  testGroup "Multiplicative Group"
  [ testProperty "recip" $
      \a -> (a::a) * (recip a) == (one::a) && (recip a) * a == (one::a)
  , testProperty "(/)" $
      \a -> (a::a) / a == (one::a)
--  , testProperty "pow" $
--      \n a ->  pow (n::Integer) (a::a) == powStd n a
--  , testProperty "(^^)" $
--      \n a ->  (a::a)^^(n :: Integer) == pow n a
  ]
