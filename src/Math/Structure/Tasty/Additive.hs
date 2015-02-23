{-# LANGUAGE
    FlexibleContexts
  , ConstraintKinds
  , ScopedTypeVariables
  #-}

module Math.Structure.Tasty.Additive where

import Prelude hiding ( (+), (-), negate, subtract )

import Data.Proxy
import Numeric.Natural ( Natural(..) )
import Test.Tasty
import Test.Natural

import Math.Structure.Additive
import Math.Structure.Additive.Semigroup ( sinnum1pStd )
import Math.Structure.Additive.Monoid ( sinnum0pStd )
import Math.Structure.Additive.Group ( sinnumStd )
import Math.Structure.Tasty.Utils


isAdditiveSemigroup :: ( Testable a, AdditiveSemigroup a )
            => Proxy a -> TestTree
isAdditiveSemigroup p = testGroup "Additive Semigroup" $
                [ isAdditiveSemigroup' p ]

isAbeleanSemigroup :: ( Testable a, Abelean a, AdditiveSemigroup a )
                   => Proxy a -> TestTree
isAbeleanSemigroup p = testGroup "Additive Abelean Semigroup" $
                       [ isAbelean' p
                       , isAdditiveSemigroup' p
                       ]

isAdditiveGroup :: ( Testable a, AdditiveGroup a )
           => Proxy a -> TestTree
isAdditiveGroup p = testGroup "Additive Group" $
            [ isAdditiveSemigroup' p
            , isAdditiveGroup' p
            ]

isAbeleanGroup :: ( Testable a, Abelean a, AdditiveGroup a )
               => Proxy a -> TestTree
isAbeleanGroup p = testGroup "Additive Abelean Group" $
            [ isAbelean' p
            , isAdditiveSemigroup' p
            , isAdditiveGroup' p
            ]


isAbelean' :: forall a .
              ( Testable a, Abelean a )
           => Proxy a -> TestTree
isAbelean' p = testProperty "Additive Abelean Class" $
                 \a b -> (a::a) + (b::a) == b + a

isAdditiveSemigroup' :: forall a .
                ( Testable a, AdditiveSemigroup a )
             => Proxy a -> TestTree
isAdditiveSemigroup' p = testGroup "Additive Semigroup Class"
  [ testProperty "associative" $
      \a b c -> (a::a) + ((b::a) + (c::a)) == (a + b) + c
  , testProperty "sinnum1p" $
      \n a ->  sinnum1p n (a::a) == sinnum1pStd n a
  ]

isAdditiveMonoid' :: forall a .
             ( Testable a, AdditiveMonoid a )
          => Proxy a -> TestTree
isAdditiveMonoid' p = testGroup "Additive Monoid"
  [ testProperty "zero" $
      \a -> (zero::a) + (a::a) == a && a + (zero::a) == a
  , testProperty "sinnum0p" $
      \n a ->  sinnum0p n (a::a) == sinnum0pStd n a
  ]

isAdditiveGroup' :: forall a .
            ( Testable a, AdditiveGroup a )
         => Proxy a -> TestTree
isAdditiveGroup' p = testGroup "Additive Group Class" $
  [ testProperty "negate" $
      \a -> (a::a) + (negate a) == (zero::a) && (negate a) + a == (zero::a)
  , testProperty "(-)" $
      \a -> (a::a) - a == (zero::a)
  , testProperty "subtract" $
      \a b -> subtract (a::a) (b::a) == b - a
  , testProperty "sinnum" $
      \n a ->  sinnum (n::Integer) (a::a) == sinnumStd n a
  ]
