module Math.Structure.Ring.Euclidean
where

import Data.Composition ( (.:) )
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , euclDegree, euclidean
                      )
import Numeric.Natural ( Natural )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.Ring

class Ring r => EuclideanDomain r where
  quotRem :: r -> r -> (r,r)
  quotRem a b = (quot a b, rem a b)
  
  quot :: r -> r -> r
  quot = fst .: quotRem

  rem :: r -> r -> r
  rem = snd .: quotRem

  euclDegree :: r -> Natural

euclidean :: (EuclideanDomain r, DecidableZero r)
          => r -> r -> [(r,r,r)]
euclidean a b = euclidean' [(a,one,zero),(b,zero,one)]

euclidean' :: (EuclideanDomain r, DecidableZero r)
           => [(r,r,r)] -> [(r,r,r)]
euclidean' es@((d,s,t):(d',s',t'):_)
  | isZero d = tail es
  | otherwise = euclidean' $ (r,s'-q*s,t'-q*t):es
    where
    (q,r) = quotRem d' d 
