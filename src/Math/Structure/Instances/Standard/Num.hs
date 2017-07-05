{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Math.Structure.Instances.Standard.Num
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P
import Data.Monoid ( (<>) )
import Data.Traversable ( forM )
import Language.Haskell.TH
import Numeric.Natural ( Natural )
import Foreign.C.Types ( CULong )

import Math.Structure.Additive
import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring
import Math.Structure.Utility.TH


mkAbelianGroupInstanceFromNum (return []) [t|Integer|]
mkCommutativeMonoidInstanceFromNum (return []) [t|Integer|]
mkCommutativeMonoidInstanceFromNonZeroNum (return []) [t|Integer|]
mkCommutativeGroupInstanceFromUnitNum
  [|id|]
  [|\(Unit a) (Unit a') -> Unit $ a * a'|]
  [|\(Unit a) n -> Unit $ if even n then one else a|]
  (return []) [t|Integer|]
mkEuclideanDomainInstanceFromIntegral (return []) [t|Integer|]

mkAbelianMonoidInstanceFromNum (return []) [t|Natural|]
mkCommutativeMonoidInstanceFromNum (return []) [t|Natural|]
mkCommutativeMonoidInstanceFromNonZeroNum (return []) [t|Natural|]
mkSemiringInstance (return []) [t|Natural|]

mkAbelianGroupInstanceFromNum (return []) [t|Rational|]
mkCommutativeMonoidInstanceFromNum (return []) [t|Rational|]
mkCommutativeGroupInstanceFromNonZeroFractional (return []) [t|Rational|]
mkCommutativeGroupInstanceFromUnitFractional (return []) [t|Rational|]
mkFieldInstance (return []) [t|Rational|]


fmap concat $ forM
  [ ([t|Int|],"Int")
  , ([t|CULong|],"CULong")
  ] $ \(ty,str) -> concat <$> sequence
    [ mkAbelianGroupInstanceFromNum (return []) ty
    , mkCommutativeMonoidInstanceFromNum (return []) ty
    , mkCommutativeMonoidInstanceFromNonZeroNum (return []) ty
    , mkCommutativeGroupInstanceFromUnitNum
        [| error $ "Unit " <> str <> ": recip" |]
        [| error $ "Unit " <> str <> ": /" |]
        [| error $ "Unit " <> str <> ": ^^" |]
        (return []) ty
    , mkRingInstance (return []) ty
    ]
