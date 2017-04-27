{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Instances.TH.Ring
where

import Control.Applicative ( (<$>) )
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , lcm, gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.Monad ( sequence, liftM, liftM2 )
import Data.Composition ( (.:) )
import Language.Haskell.TH

import Math.Structure.Additive.DecidableZero
import Math.Structure.Ring
import Math.Structure.Utility.TH


mkSemiringInstance :: CxtQ -> TypeQ -> DecsQ
mkSemiringInstance cxt r = sequence
  [ mkInstance cxt r [t|Semiring|]
  , mkInstance cxt r [t|Distributive|]
  ]

mkRingInstance :: CxtQ -> TypeQ -> DecsQ
mkRingInstance cxt r = concat <$> sequence
  [ mkSemiringInstance cxt r
  , sequence 
    [ mkInstance cxt r [t|Rng|]
    , mkInstance cxt r [t|Rig|]
    , mkInstance cxt r [t|Ring|]
    ]
  ]

mkFieldInstance :: CxtQ -> TypeQ -> DecsQ
mkFieldInstance cxt r = concat <$> sequence
  [ mkRingInstance cxt r
  , sequence
    [ mkInstance cxt r [t|IntegralDomain|]
    , mkInstance cxt r [t|DivisionRing|]
    , mkInstance cxt r [t|Field|]
    ]
  ]

mkEuclideanDomainInstanceFromIntegral :: CxtQ -> TypeQ -> DecsQ
mkEuclideanDomainInstanceFromIntegral cxt r =
  liftM mconcat $ sequence
  [ mkRingInstance cxt r
  , mkEuclideanDomainInstanceFromIntegral' cxt r [| P.gcd |] [| head .: euclidean |] [| P.lcm |]
  ]

mkEuclideanDomainInstanceFromIntegralWithCustomGCD :: CxtQ -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> DecsQ
mkEuclideanDomainInstanceFromIntegralWithCustomGCD cxt r gcdD xgcdD lcmD =
  liftM mconcat $ sequence
  [ mkRingInstance cxt r
  , mkEuclideanDomainInstanceFromIntegral' cxt r gcdD xgcdD lcmD
  ]

mkEuclideanDomainInstanceFromIntegral' :: CxtQ -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> DecsQ
mkEuclideanDomainInstanceFromIntegral' cxt r gcdD xgcdD lcmD = sequence
  [ mkInstance cxt r [t|IntegralDomain|]
  , mkInstanceWith cxt r [t|PIDomain|]
      [ mkDecl 'gcd gcdD
      , mkDecl 'xgcd xgcdD
      , mkDecl 'lcm lcmD
      ]
  , mkInstanceWith cxt r [t|EuclideanDomain|]
      [ mkDecl 'quotRem [| P.quotRem |]
      , mkDecl 'euclNorm
          [| \a -> if a==0
                   then Nothing
                   else Just $ P.fromIntegral (P.abs a) |]
      ]
  ]
