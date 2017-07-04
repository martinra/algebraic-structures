{-# LANGUAGE
    FlexibleContexts
  , ConstraintKinds
  , ScopedTypeVariables
  #-}

module Math.Structure.Tasty.Additive where

import Prelude hiding ( (+), (-), negate, subtract )

import Control.Monad.Reader ( ask )
import Data.Proxy
import Numeric.Natural ( Natural(..) )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Natural
import Test.QuickCheck.Arbitrary ()

import Math.Structure.Additive
import Math.Structure.Additive.Semigroup ( sinnum1pStd )
import Math.Structure.Additive.Monoid ( sinnum0pStd )
import Math.Structure.Additive.Group ( sinnumStd )
import Math.Structure.Utility.Tasty


isAdditiveSemigroup :: ( Testable a, AdditiveSemigroup a )
            => Proxy a -> TestR [TestTree]
isAdditiveSemigroup p = sequence
  [ isAdditiveSemigroup' p ]

isAbelianSemigroup :: ( Testable a, Abelian a, AdditiveSemigroup a )
                   => Proxy a -> TestR [TestTree]
isAbelianSemigroup p = sequence
  [ isAbelian' p
  , isAdditiveSemigroup' p
  ]

isAdditiveGroup :: ( Testable a, AdditiveGroup a )
           => Proxy a -> TestR [TestTree]
isAdditiveGroup p = sequence
  [ isAdditiveSemigroup' p
  , isAdditiveGroup' p
  ]

isAbelianGroup :: ( Testable a, AbelianGroup a )
               => Proxy a -> TestR [TestTree]
isAbelianGroup p = sequence
  [ isAbelian' p
  , isAdditiveSemigroup' p
  , isAdditiveGroup' p
  ]

hasDecidableZero :: forall a.
                    ( Testable a, DecidableZero a )
                 => Proxy a -> TestR [TestTree]
hasDecidableZero p = withTestProperty $ \testProperty ->
  [ testGroup "Decidable Zero" $
    [ testCase "isZero zero" $ isZero (zero::a) @?= True
    , testProperty "isZero <=> (==zero)" $
        \a -> ((a::a) == zero) == (isZero a)
    ]
  ]


isAbelian' :: forall a .
              ( Testable a, Abelian a )
           => Proxy a -> TestR TestTree
isAbelian' p = withTestProperty $ \testProperty ->
               testProperty "Additive Abelian" $
                 \a b -> (a::a) + (b::a) == b + a

isAdditiveSemigroup' :: forall a .
                ( Testable a, AdditiveSemigroup a )
             => Proxy a -> TestR TestTree
isAdditiveSemigroup' p = withTestProperty $ \testProperty ->
  testGroup "Additive Semigroup"
  [ testProperty "associative" $
      \a b c -> (a::a) + ((b::a) + (c::a)) == (a + b) + c
  , testProperty "sinnum1p" $
      \n a ->  sinnum1p n (a::a) == sinnum1pStd n a
  ]

isAdditiveMonoid' :: forall a .
             ( Testable a, AdditiveMonoid a )
          => Proxy a -> TestR TestTree
isAdditiveMonoid' p = withTestProperty $ \testProperty ->
  testGroup "Additive Monoid"
  [ testProperty "zero" $
      \a -> (zero::a) + (a::a) == a && a + (zero::a) == a
  , testProperty "sinnum0p" $
      \n a ->  sinnum0p n (a::a) == sinnum0pStd n a
  ]

isAdditiveGroup' :: forall a .
            ( Testable a, AdditiveGroup a )
         => Proxy a -> TestR TestTree
isAdditiveGroup' p = withTestProperty $ \testProperty ->
  testGroup "Additive Group" $
  [ testProperty "negate" $
      \a -> (a::a) + (negate a) == (zero::a) && (negate a) + a == (zero::a)
  , testProperty "(-)" $
      \a -> (a::a) - a == (zero::a)
  , testProperty "subtract" $
      \a b -> subtract (a::a) (b::a) == b - a
  , testProperty "sinnum" $
      \n a ->  sinnum (n::Integer) (a::a) == sinnumStd n a
  ]
