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
import Math.Structure.Utils.TH


-- | Make commutative group instance of n, assuming Num n and zero != one
mkCommutativeMonoidInstanceFromNum :: Name -> DecsQ
mkCommutativeMonoidInstanceFromNum n = sequence
  [ mkInstanceWith' n ''MultiplicativeMagma
      [ mkDecl '(*) [| (P.*) |] ]
  , mkInstance' n ''Commutative
  , mkInstance' n ''MultiplicativeSemigroup 
  , mkInstanceWith' n ''MultiplicativeMonoid
      [ mkDecl 'one [| 1 |]
      , mkDecl '(^) [| (P.^) |]
      ]
  , mkInstanceWith' n ''DecidableOne
      [ mkDecl 'isOne [| (==1) |] ]
  ]

mkCommutativeMonoidInstanceFromNonZeroNum :: Name -> DecsQ
mkCommutativeMonoidInstanceFromNonZeroNum n = sequence
  [ deriveInstance nonzeroR (conT ''DecidableOne)
  , deriveInstance nonzeroR (conT ''MultiplicativeMonoid)
  ]
  where
  nonzeroR = appT (conT ''NonZero) (conT n)

mkCommutativeGroupInstanceFromNonZeroFractional :: Name -> DecsQ
mkCommutativeGroupInstanceFromNonZeroFractional n = concat <$> sequence
  [ mkCommutativeMonoidInstanceFromNum n
  , mkCommutativeMonoidInstanceFromNonZeroNum n
  , sequence 
    [ mkInstanceWith nonzeroR (conT ''MultiplicativeGroup)
        [ mkDecl 'recip [| \(NonZero a) -> NonZero $ P.recip a |]
        , mkDecl '(/) [| \(NonZero a) (NonZero b) -> NonZero $ a P./ b |]
        , mkDecl '(^^) [| \(NonZero a) n ->  NonZero $ a P.^^ n |]
        ] 
    ]
  ]
  where
  nonzeroR = appT (conT ''NonZero) (conT n)
