{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Ring.StdInstances
where

import Math.Structure.Ring.TH
import Math.Structure.Additive.StdInstances
import Math.Structure.Multiplicative.StdInstances


mkPIDRingInstanceFromIntegral ''Integer
mkPIDRingInstanceFromIntegral ''Int
