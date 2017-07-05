{-# LANGUAGE TemplateHaskell #-}

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
mkCommutativeMonoidInstanceFromNum cxt ty = sequence
  [ mkInstanceWith cxt ty [t|MultiplicativeMagma|]
      [ mkDecl '(*) [| (P.*) |] ]
  , mkInstance cxt ty [t|Commutative|]
  , mkInstance cxt ty [t|MultiplicativeSemigroup|]
  , mkInstanceWith cxt ty [t|MultiplicativeMonoid|]
      [ mkDecl 'one [| 1 |]
      , mkDecl '(^) [| (P.^) |]
      ]
  , mkInstanceWith cxt ty [t|DecidableOne|]
      [ mkDecl 'isOne [| (==1) |] ]
  ]

mkCommutativeMonoidInstanceDerived :: CxtQ -> TypeQ -> DecsQ
mkCommutativeMonoidInstanceDerived ctx ty = sequence
  [ deriveInstance ctx ty (conT ''MultiplicativeMagma)
  , deriveInstance ctx ty (conT ''MultiplicativeSemigroup)
  , deriveInstance ctx ty (conT ''MultiplicativeMonoid)
  , deriveInstance ctx ty (conT ''DecidableOne)
  , deriveInstance ctx ty (conT ''Commutative)
  ]

mkCommutativeMonoidInstanceFromNonZeroNum :: CxtQ -> TypeQ -> DecsQ
mkCommutativeMonoidInstanceFromNonZeroNum ctx ty =
  mkCommutativeMonoidInstanceDerived ctx $ appT (conT ''NonZero) ty

mkCommutativeGroupInstanceFromNonZeroFractional :: CxtQ -> TypeQ -> DecsQ
mkCommutativeGroupInstanceFromNonZeroFractional cxt ty = concat <$> sequence
  [ mkCommutativeMonoidInstanceFromNonZeroNum cxt ty
  , sequence 
    [ mkInstanceWith cxt (appT (conT ''NonZero) ty) (conT ''MultiplicativeGroup)
        [ mkDecl 'recip [| \(NonZero a) -> NonZero $ P.recip a |]
        , mkDecl '(/)   [| \(NonZero a) (NonZero b) -> NonZero $ a P./ b |]
        , mkDecl '(^^)  [| \(NonZero a) n ->  NonZero $ a P.^^ n |]
        ] 
    ]
  ]

mkCommutativeGroupInstanceFromUnitNum
  :: ExpQ -> ExpQ -> ExpQ
  -> CxtQ -> TypeQ -> DecsQ
mkCommutativeGroupInstanceFromUnitNum recipNum quotNum powNum cxt ty = sequence
  [ mkInstanceWith cxt (appT (conT ''Unit) ty) (conT ''MultiplicativeGroup)
      [ mkDecl 'recip recipNum
      , mkDecl '(/)   quotNum
      , mkDecl '(^^)  powNum
      ]
  ]

mkCommutativeGroupInstanceFromUnitFractional :: CxtQ -> TypeQ -> DecsQ
mkCommutativeGroupInstanceFromUnitFractional cxt ty = sequence
  [ mkInstanceWith cxt (appT (conT ''Unit) ty) (conT ''MultiplicativeGroup)
      [ mkDecl 'recip [| \(Unit a) -> Unit $ P.recip a |]
      , mkDecl '(/)   [| \(Unit a) (Unit b) -> Unit $ a P./ b |]
      , mkDecl '(^^)  [| \(Unit a) n ->  Unit $ a P.^^ n |]
      ]
  ]
