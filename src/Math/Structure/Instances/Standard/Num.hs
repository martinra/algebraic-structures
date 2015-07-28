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
import Foreign.C.Types ( CULong )

import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring
import Math.Structure.Utils.TH


mkAbelianGroupInstanceFromNum (return []) [t|Integer|]
mkCommutativeMonoidInstanceFromNum (return []) [t|Integer|]
mkCommutativeMonoidInstanceFromNonZeroNum (return []) [t|Integer|]
mkEuclideanDomainInstanceFromIntegral (return []) [t|Integer|]

mkAbelianGroupInstanceFromNum (return []) [t|Int|]
mkCommutativeMonoidInstanceFromNum (return []) [t|Int|]
mkCommutativeMonoidInstanceFromNonZeroNum (return []) [t|Int|]
mkRingInstance (return []) [t|Int|]
--mkEuclideanDomainInstanceFromIntegral (return []) [t|Int|]

mkAbelianMonoidInstanceFromNum (return []) [t|Natural|]
mkCommutativeMonoidInstanceFromNum (return []) [t|Natural|]
mkSemiringInstance (return []) [t|Natural|]

mkAbelianGroupInstanceFromNum (return []) [t|Rational|]
mkCommutativeGroupInstanceFromNonZeroFractional (return []) [t|Rational|]
mkFieldInstance (return []) [t|Rational|]


mkAbelianGroupInstanceFromNum (return []) [t|CULong|]
mkCommutativeMonoidInstanceFromNum (return []) [t|CULong|]
mkCommutativeMonoidInstanceFromNonZeroNum (return []) [t|CULong|]
mkRingInstance (return []) [t|CULong|]
