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

import Math.Structure.Multiplicative
import Math.Structure.Multiplicative.Semigroup ( pow1pStd )
import Math.Structure.Multiplicative.Monoid ( pow0pStd )
import Math.Structure.Multiplicative.Group ( powStd )
import Math.Structure.Utils.Tasty


isMultiplicativeSemigroup ::
     ( Testable a, MultiplicativeSemigroup a )
  => Proxy a -> [TestTree]
isMultiplicativeSemigroup p =
  [ isMultiplicativeSemigroup' p ]

isCommutativeSemigroup ::
     ( Testable a, Commutative a, MultiplicativeSemigroup a )
  => Proxy a -> [TestTree]
isCommutativeSemigroup p =
  [ isCommutative' p
  , isMultiplicativeSemigroup' p
  ]

isMultiplicativeMonoid ::
     ( Testable a, MultiplicativeMonoid a )
  => Proxy a -> [TestTree]
isMultiplicativeMonoid p =
  [ isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  ]

isCommutativeMonoid ::
     ( Testable a, Commutative a, MultiplicativeMonoid a )
  => Proxy a -> [TestTree]
isCommutativeMonoid p =
  [ isCommutative' p
  , isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  ]

isMultiplicativeGroup ::
     ( Testable a, MultiplicativeGroup a )
  => Proxy a -> [TestTree]
isMultiplicativeGroup p =
  [ isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  , isMultiplicativeGroup' p
  ]

isCommutativeGroup ::
    ( Testable a, CommutativeGroup a )
  => Proxy a -> [TestTree]
isCommutativeGroup p =
  [ isCommutative' p
  , isMultiplicativeSemigroup' p
  , isMultiplicativeMonoid' p
  , isMultiplicativeGroup' p
  ]

hasDecidableOne :: forall a.
     ( Testable a, DecidableOne a )
  => Proxy a -> [TestTree]
hasDecidableOne p = (:[]) $ testGroup "Decidable One" $
  [ testCase "isOne one" $ isOne (one::a) @?= True
  , testProperty "isOne <=> (==one)" $
      \a -> ((a::a) == one) == (isOne a)
  ]

isMultiplicativeLeftAction :: forall g s.
     ( Testable g, Testable s, MultiplicativeLeftAction g s )
  => Proxy g -> Proxy s -> [TestTree]
isMultiplicativeLeftAction pg ps = (:[]) $
  testGroup "Multiplicative Left Action" $
  [ testProperty "Semigroup action" $
      \g h s -> ((g::g) * (h::g)) *. (s::s) == g *. (h *. s)
  , testProperty "Monoid action" $
      \s -> (one::g) *. (s::s) == s
  ]

isMultiplicativeRightAction :: forall g s.
     ( Testable g, Testable s, MultiplicativeRightAction g s )
  => Proxy g -> Proxy s -> [TestTree]
isMultiplicativeRightAction pg ps = (:[]) $
  testGroup "Multiplicative Right Action" $
  [ testProperty "Semigroup action" $
      \g h s -> (s::s) .* ((g::g) * (h::g)) == (s .* g) .* h
  , testProperty "Monoid action" $
      \s -> (s::s) .* (one::g) == s
  ]


isCommutative' :: forall a .
     ( Testable a, Commutative a )
  => Proxy a -> TestTree
isCommutative' p = testProperty "Multiplicative Commutative Class" $
                 \a b -> (a::a) * (b::a) == b * a

isMultiplicativeSemigroup' :: forall a .
     ( Testable a, MultiplicativeSemigroup a )
  => Proxy a -> TestTree
isMultiplicativeSemigroup' p = testGroup "Multiplicative Semigroup Class"
  [ testProperty "associative" $
      \a b c -> (a::a) * ((b::a) * (c::a)) == (a * b) * c
  , testProperty "pow1p" $
      \n a ->  pow1p n (a::a) == pow1pStd n a
  ]

isMultiplicativeMonoid' :: forall a .
     ( Testable a, MultiplicativeMonoid a )
  => Proxy a -> TestTree
isMultiplicativeMonoid' p = testGroup "Multiplicative Monoid"
  [ testProperty "one" $
      \a -> (one::a) * (a::a) == a && a * (one::a) == a
  , testProperty "pow0p" $
      \n a ->  pow0p n (a::a) == pow0pStd n a
  , testProperty "(^)" $
      \n a ->  (a::a)^(toInteger (n :: Natural)) == pow0pStd n a
  ]

isMultiplicativeGroup' :: forall a .
     ( Testable a, MultiplicativeGroup a )
  => Proxy a -> TestTree
isMultiplicativeGroup' p = testGroup "Multiplicative Group Class" $
  [ testProperty "recip" $
      \a -> (a::a) * (recip a) == (one::a) && (recip a) * a == (one::a)
  , testProperty "(/)" $
      \a -> (a::a) / a == (one::a)
  , testProperty "pow" $
      \n a ->  pow (n::Integer) (a::a) == powStd n a
  , testProperty "(^^)" $
      \n a ->  (a::a)^^(n :: Integer) == pow n a
  ]
