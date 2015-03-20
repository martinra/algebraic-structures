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

import Math.Structure.Ring
import Math.Structure.Utils.Tasty

import Math.Structure.Tasty.Additive
import Math.Structure.Tasty.Multiplicative
import Math.Structure.Tasty.NonZero


isRing ::
     ( Testable r, Ring r )
  => Proxy r -> [TestTree]
isRing p =
  isAbeleanGroup p ++
  isMultiplicativeMonoid p ++
  [ isDistributive' p ]

isEuclideanDomain :: ( Testable a, EuclideanDomain a, DecidableZero a )
                  => Proxy a -> [TestTree]
isEuclideanDomain p =
  isAbeleanGroup p ++
  isCommutativeMonoid p ++
  [ isDistributive' p
  , isIntegralDomain' p
  , isPIDomain' p
  , isEuclideanDomain' p
  ]

isField :: forall a .
           ( Testable a, Field a, DecidableZero a )
        => Proxy a -> [TestTree]
isField p =
  isAbeleanGroup p ++
  isCommutativeGroup (Proxy::Proxy (NonZero a)) ++
  [ isDistributive' p
  , isIntegralDomain' p
  ]


isDistributive' :: forall a .
                   ( Testable a, Distributive a )
                => Proxy a -> TestTree
isDistributive' p = testProperty "Distributive Magmas" $
  \a b c -> (a::a) * ((b::a) + (c::a)) == a*b + a*c &&
            (a + b) * c == a*c + b*c

isIntegralDomain' :: forall a .
                     ( Testable a, IntegralDomain a )
                  => Proxy a -> TestTree
isIntegralDomain' p = testProperty "IntegralDomain" $
  \a b -> ((a::a)*(b::a)==zero) == (a==zero || b==zero)
        
isPIDomain' :: forall a .
          ( Testable a, PIDomain a )
       => Proxy a -> TestTree
isPIDomain' p = testGroup "PID"
  [ testProperty "fst . xgcd = gcd" $
      \a b -> let (d,_,_) = xgcd (a::a) (b::a)
              in gcd a b == d
  , testProperty "xgcd" $
      \a b -> let (d,s,t) = xgcd (a::a) (b::a)
              in d == a*s + b*t
  ]

isEuclideanDomain' :: forall a .
                      ( Testable a, EuclideanDomain a, DecidableZero a )
                   => Proxy a -> TestTree

isEuclideanDomain' p = testGroup "Euclidean Domain"
  [ testProperty "quotRem" $
      \a b -> let b' = fromNonZero b :: a
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
