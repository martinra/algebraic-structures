{-# LANGUAGE
    TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Math.Structure.Instances.Standard.Ring
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      )
import qualified Prelude as P
import Numeric.Natural ( Natural )

import Math.Structure.Instances.Standard.Additive
import Math.Structure.Instances.Standard.Multiplicative
import Math.Structure.Instances.TH.Ring


mkEuclideanDomainInstanceFromIntegral ''Integer
mkEuclideanDomainInstanceFromIntegral ''Int
mkSemiringInstance ''Natural

mkFieldInstance ''Rational
