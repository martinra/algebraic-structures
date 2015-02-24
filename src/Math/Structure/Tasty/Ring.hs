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

import Data.Proxy
import Numeric.Natural ( Natural(..) )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Natural

import Math.Structure.Ring
import Math.Structure.Utils.Tasty

import Math.Structure.Tasty.Additive
import Math.Structure.Tasty.Multiplicative


isEuclideanDomain :: ( Testable a, EuclideanDomain a )
                  => Proxy a -> [TestTree]
isEuclideanDomain p =
  isAbeleanGroup p ++
  isCommutativeMonoid p ++
  [ isDistributive' p
  , isIntegralDomain' p
  , isPID' p
  , isEuclideanDomain' p
  ]

isField :: ( Testable a, Field a )
        => Proxy a -> [TestTree]
isField p =
  isAbeleanGroup p ++
  isCommutativeGroup p ++
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
        
isPID' :: forall a .
          ( Testable a, PID a )
       => Proxy a -> TestTree
isPID' p = testGroup "PID"
  [ testProperty "fst . xgcd = gcd" $
      \a b -> let (d,_,_) = xgcd (a::a) (b::a)
              in gcd a b == d
  , testProperty "xgcd" $
      \a b -> let (d,s,t) = xgcd (a::a) (b::a)
              in d == a*s + b*t
  ]

isEuclideanDomain' :: forall a .
                      ( Testable a, EuclideanDomain a )
                   => Proxy a -> TestTree

isEuclideanDomain' p = testGroup "Euclidean Domain"
  [ testProperty "quotRem" $
      \a b -> let (q,r) = quotRem (a::a) (b::a)
              in r + q*b == a
  , testProperty "quot & rem" $
      \a b -> (quot a b, rem a b) == quotRem (a::a) (b::a)
  , testProperty "euclDegree" $
      \a -> (euclDegree (a::a) == 0) == (a==zero)
  , testProperty "euclDegree for quotRem" $
      \a b -> let (q,r) = quotRem (a::a) (b::a)
              in (b==zero) || (euclDegree r < euclDegree b)
  ]
