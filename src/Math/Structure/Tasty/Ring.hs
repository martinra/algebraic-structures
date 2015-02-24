{-# LANGUAGE
    FlexibleContexts
  , ConstraintKinds
  , ScopedTypeVariables
  #-}

module Math.Structure.Tasty.Ring where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )

import Data.Proxy
import Numeric.Natural ( Natural(..) )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Natural

import Math.Structure.Ring
import Math.Structure.Utils.Tasty


isEuclideanDomain :: ( Testable a, EuclideanDomain a )
                  => Proxy a -> [TestTree]
isEuclideanDomain p =
  isAbeleanGroup p ++
  isCommutativeMonoid p ++
  [ isDistributive' p
  , isRng' p
  , isRig' p
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

isPID' :: forall a .
          ( Testable a, PID a )
       => Proxy a -> TestTree
isPID' p = testGroup "PID"
  [ \a b -> fst (xgcd (a::a) (b::a)) == gcd a b
  , \a b -> let (d,s,t) = xgcd (a::a) (b::a)
            in d == a*s + b*t
  ]

isEuclideanDomain' :: forall a .
                      ( Testable a, EuclideanDomain a )
                   => Proxy a -> TestTree

isEuclideanDomain' p = testGroup "Euclidean Domain"
  [ \a b -> let (q,r) = quotRem (a::a) (b::a)

            in r + q*b == a
  , \a b -> (quot a b, rem a b) == quotRem (a::a) (b::a)
  , \a -> (euclDegree (a::a) == 0) == (a==0)
  , \a b -> let (q,r) = quotRem (a::a) (b::a)
            in (b==0) || (euclDegree r < euclDegree b)
  ]
