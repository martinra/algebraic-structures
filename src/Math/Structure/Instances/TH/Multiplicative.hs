{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Instances.TH.Multiplicative
where

import Prelude hiding ( (*), (/), recip, (^), (^^) )
import qualified Prelude as P

import Control.Applicative ( (<$>) )
import Language.Haskell.TH

import Math.Structure.Multiplicative
import Math.Structure.Utils.TH


-- | Make commutative group instance of n, assuming Num n
mkCommutativeMonoidInstanceFromNum :: Name -> DecsQ
mkCommutativeMonoidInstanceFromNum n = sequence
  [ mkInstanceWith n ''MultiplicativeMagma
      [ mkDecl '(*) [| (P.*) |] ]
  , mkInstance n ''Commutative
  , mkInstance n ''MultiplicativeSemigroup 
  , mkInstanceWith n ''MultiplicativeMonoid
      [ mkDecl 'one [| 1 |]
      , mkDecl '(^) [| (P.^) |]
      ]
  , mkInstanceWith n ''DecidableOne
      [ mkDecl 'isOne [| (==1) |] ]
  ]

mkCommutativeGroupInstanceFromFractional :: Name -> DecsQ
mkCommutativeGroupInstanceFromFractional n = concat <$> sequence
  [ mkCommutativeMonoidInstanceFromNum n
  , sequence 
    [ mkInstanceWith n ''MultiplicativeGroup
        [ mkDecl 'recip [| P.recip |]
        , mkDecl '(/) [| (P./) |]
        , mkDecl '(^^) [| (P.^^) |]
        ] 
    ]
  ]
