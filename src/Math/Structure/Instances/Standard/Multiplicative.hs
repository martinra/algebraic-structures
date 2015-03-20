{-# LANGUAGE
    TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Math.Structure.Instances.Standard.Multiplicative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P
import Numeric.Natural ( Natural )

import Math.Structure.Instances.TH.Multiplicative


mkCommutativeMonoidInstanceFromNum ''Integer
mkCommutativeMonoidInstanceFromNum ''Int
mkCommutativeMonoidInstanceFromNum ''Natural

mkCommutativeGroupInstanceFromFractional ''Rational
