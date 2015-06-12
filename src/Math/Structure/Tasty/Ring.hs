{-# LANGUAGE
    FlexibleContexts
  , ConstraintKinds
  , ScopedTypeVariables
  #-}

module Math.Structure.Tasty.Ring where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Maybe
import Data.Proxy
import Numeric.Natural ( Natural(..) )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Natural
import Test.QuickCheck.Arbitrary ()

import Math.Structure.Ring
import Math.Structure.Utils.Tasty

import Math.Structure.Tasty.Additive
import Math.Structure.Tasty.Multiplicative
import Math.Structure.Tasty.NonZero


isSemiring ::
     ( Testable r, Semiring r )
  => Proxy r -> TestR [TestTree]
isSemiring p = fmap concat $ sequence
  [ isAbelianSemigroup p
  , isMultiplicativeSemigroup p
  , sequence [ isDistributive' p ]
  ]

isRng ::
     ( Testable r, Rng r )
  => Proxy r -> TestR [TestTree]
isRng p = fmap concat $ sequence
  [ isAbelianGroup p
  , isMultiplicativeSemigroup p
  , sequence [ isDistributive' p ]
  ]

isRing ::
     ( Testable r, Ring r )
  => Proxy r -> TestR [TestTree]
isRing p = fmap concat $ sequence
  [ isAbelianGroup p
  , isMultiplicativeMonoid p
  , sequence [ isDistributive' p ]
  ]

isEuclideanDomain :: ( Testable a, Testable (NonZero a)
                     , EuclideanDomain a, DecidableZero a )
                  => Proxy a -> TestR [TestTree]
isEuclideanDomain p = fmap concat $ sequence
  [ isAbelianGroup p
  , isCommutativeMonoid p
  , sequence
    [ isDistributive' p
    , isIntegralDomain' p
    , isPIDomain' p
    , isEuclideanDomain' p
    ]
  ]

isField :: forall a .
           ( Testable a, Testable (NonZero a)
           , Field a, DecidableZero a )
        => Proxy a -> TestR [TestTree]
isField p = fmap concat $ sequence
  [ isAbelianGroup p
  , isCommutativeGroup (Proxy::Proxy (NonZero a))
  , sequence
    [ isDistributive' p
    , isIntegralDomain' p
    ]
  ]


isDistributive' :: forall a .
                   ( Testable a, Distributive a )
                => Proxy a -> TestR TestTree
isDistributive' p = withTestProperty $ \testProperty ->
  testProperty "Distributive Magmas" $
  \a b c -> (a::a) * ((b::a) + (c::a)) == a*b + a*c &&
            (a + b) * c == a*c + b*c

isIntegralDomain' :: forall a .
                     ( Testable a, IntegralDomain a )
                  => Proxy a -> TestR TestTree
isIntegralDomain' p = withTestProperty $ \testProperty ->
  testProperty "IntegralDomain" $
  \a b -> ((a::a)*(b::a)==zero) == (a==zero || b==zero)
        
isPIDomain' :: forall a .
          ( Testable a, PIDomain a )
       => Proxy a -> TestR TestTree
isPIDomain' p = withTestProperty $ \testProperty ->
  testGroup "PID"
  [ testProperty "fst . xgcd = gcd" $
      \a b -> let (d,_,_) = xgcd (a::a) (b::a)
              in gcd a b == d
  , testProperty "xgcd" $
      \a b -> let (d,s,t) = xgcd (a::a) (b::a)
              in d == a*s + b*t
  ]

isEuclideanDomain' :: forall a .
                      ( Testable a, Testable (NonZero a)
                      , EuclideanDomain a, DecidableZero a )
                   => Proxy a -> TestR TestTree
isEuclideanDomain' p = withTestProperty $ \testProperty ->
  testGroup "Euclidean Domain"
  [ testProperty "quotRem" $
      \a b -> let b' = fromNonZero (b :: NonZero a)
                  (q,r) = quotRem (a::a) b'
              in r + q*b' == a
  , testProperty "quot & rem" $
      \a b -> let b' = fromNonZero b :: a
              in (quot a b', rem a b') == quotRem (a::a) b'
  , testProperty "euclNorm" $
      \a -> (isNothing $ euclNorm (a::a)) == (a==zero)
  , testProperty "euclNorm for quotRem" $
      \a b -> let  b' = fromNonZero b :: a
                   (q,r) = quotRem (a::a) b'
              in maybe True (fromJust (euclNorm b') >)
                         (euclNorm r)
  ]
