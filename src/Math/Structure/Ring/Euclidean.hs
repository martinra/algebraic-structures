module Math.Structure.Ring.Euclidean
where

import Data.Composition ( (.:) )
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , lcm, gcd
                      , quotRem, quot, rem
                      )
import Numeric.Natural ( Natural )

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring.PID

class PIDomain r => EuclideanDomain r where
  quotRem :: r -> r -> (r,r)
  quotRem a b = (quot a b, rem a b)
  
  quot :: r -> r -> r
  quot = fst .: quotRem

  rem :: r -> r -> r
  rem = snd .: quotRem

  euclNorm :: r -> Maybe Natural

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

xlcm :: EuclideanDomain r => r -> r -> (r,r,r)
xlcm a b =
  let c = lcm a b
  in  (c, c `quot` a, c `quot` b)
