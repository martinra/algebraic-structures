{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Instances.TH.Ring
where

import Control.Applicative ( (<$>) )
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.Monad ( sequence, liftM, liftM2 )
import Data.Composition ( (.:) )
import Language.Haskell.TH

import Math.Structure.Additive.DecidableZero
import Math.Structure.Ring
import Math.Structure.Utils.TH


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
  , mkEuclideanDomainInstanceFromIntegral' cxt r
  ]

mkEuclideanDomainInstanceFromIntegral' :: CxtQ -> TypeQ -> DecsQ
mkEuclideanDomainInstanceFromIntegral' cxt r = sequence
  [ mkInstance cxt r [t|IntegralDomain|]
  , mkInstanceWith cxt r [t|PIDomain|]
      [ mkDecl 'gcd [| P.gcd |]
      , mkDecl 'xgcd [| head .: euclidean |]
      ]
  , mkInstanceWith cxt r [t|EuclideanDomain|]
      [ mkDecl 'quotRem [| P.quotRem |]
      , mkDecl 'euclNorm
          [| \a -> if a==0
                   then Nothing
                   else Just $ P.fromIntegral (P.abs a) |]
      ]
  ]
