{-# LANGUAGE
    TemplateHaskell
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}

module Math.Structure.Instances.TH.Multiplicative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P

import Control.Applicative ( (<$>) )
import Language.Haskell.TH

import Math.Structure.Additive.DecidableZero
import Math.Structure.Multiplicative
import Math.Structure.Utility.TH


-- | Make commutative group instance of n, assuming Num n and zero != one
mkCommutativeMonoidInstanceFromNum :: CxtQ -> TypeQ -> DecsQ
mkCommutativeMonoidInstanceFromNum cxt t = sequence
  [ mkInstanceWith cxt t [t|MultiplicativeMagma|]
      [ mkDecl '(*) [| (P.*) |] ]
  , mkInstance cxt t [t|Commutative|]
  , mkInstance cxt t [t|MultiplicativeSemigroup|]
  , mkInstanceWith cxt t [t|MultiplicativeMonoid|]
      [ mkDecl 'one [| 1 |]
      , mkDecl '(^) [| (P.^) |]
      ]
  , mkInstanceWith cxt t [t|DecidableOne|]
      [ mkDecl 'isOne [| (==1) |] ]
  ]

mkCommutativeMonoidInstanceFromNonZeroNum :: CxtQ -> TypeQ -> DecsQ
mkCommutativeMonoidInstanceFromNonZeroNum cxt t = sequence
  [ deriveInstance cxt nonzeroR (conT ''MultiplicativeMagma)
  , deriveInstance cxt nonzeroR (conT ''MultiplicativeSemigroup)
  , deriveInstance cxt nonzeroR (conT ''MultiplicativeMonoid)
  , deriveInstance cxt nonzeroR (conT ''DecidableOne)
  , deriveInstance cxt nonzeroR (conT ''Commutative)
  ]
  where
  nonzeroR = appT (conT ''NonZero) t

mkCommutativeGroupInstanceFromNonZeroFractional :: CxtQ -> TypeQ -> DecsQ
mkCommutativeGroupInstanceFromNonZeroFractional cxt t = concat <$> sequence
  [ mkCommutativeMonoidInstanceFromNum cxt t
  , mkCommutativeMonoidInstanceFromNonZeroNum cxt t
  , sequence 
    [ mkInstanceWith cxt nonzeroR (conT ''MultiplicativeGroup)
        [ mkDecl 'recip [| \(NonZero a) -> NonZero $ P.recip a |]
        , mkDecl '(/) [| \(NonZero a) (NonZero b) -> NonZero $ a P./ b |]
        , mkDecl '(^^) [| \(NonZero a) n ->  NonZero $ a P.^^ n |]
        ] 
    ]
  ]
  where
  nonzeroR = appT (conT ''NonZero) t
