{-# LANGUAGE
    TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Math.Structure.Instances.Standard.Multiplicative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P

import Math.Structure.Instances.TH.Multiplicative


mkCommutativeMonoidInstanceFromNum ''Integer
mkCommutativeMonoidInstanceFromNum ''Int

mkCommutativeGroupInstanceFromFractional ''Rational
