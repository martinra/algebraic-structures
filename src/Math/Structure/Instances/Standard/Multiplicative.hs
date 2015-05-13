{-# LANGUAGE
    TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Math.Structure.Instances.Standard.Multiplicative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P
import Numeric.Natural ( Natural )

import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Instances.TH.Multiplicative


mkCommutativeMonoidInstanceFromNum ''Integer
mkCommutativeMonoidInstanceFromNum ''Int
mkCommutativeMonoidInstanceFromNum ''Natural

mkCommutativeGroupInstanceFromFractional ''Rational
deriving instance MultiplicativeGroup (NonZero Rational)
