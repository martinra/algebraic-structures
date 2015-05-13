{-# LANGUAGE
    TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Math.Structure.Instances.Standard.Num
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P
import Language.Haskell.TH
import Numeric.Natural ( Natural )

import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring
import Math.Structure.Utils.TH


mkAbelianGroupInstanceFromNum ''Integer
mkCommutativeMonoidInstanceFromNum ''Integer
mkCommutativeMonoidInstanceFromNonZeroNum ''Integer
mkEuclideanDomainInstanceFromIntegral ''Integer

mkAbelianGroupInstanceFromNum ''Int
mkCommutativeMonoidInstanceFromNum ''Int
mkCommutativeMonoidInstanceFromNonZeroNum ''Int
mkEuclideanDomainInstanceFromIntegral ''Int

mkAbelianMonoidInstanceFromNum ''Natural
mkCommutativeMonoidInstanceFromNum ''Natural
mkSemiringInstance ''Natural

mkAbelianGroupInstanceFromNum ''Rational
mkCommutativeGroupInstanceFromNonZeroFractional ''Rational
mkFieldInstance ''Rational
