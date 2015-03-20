{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Instances.TH.Additive
where

import Control.Applicative ( (<$>) )
import Prelude hiding ( (+), (-), negate, subtract )
import qualified Prelude as P

import Language.Haskell.TH

import Math.Structure.Additive
import Math.Structure.Utils.TH


-- | Make abelean monoid instance of n, assuming Num n
mkAbelianMonoidInstanceFromNum :: Name -> DecsQ
mkAbelianMonoidInstanceFromNum n = sequence
  [ mkInstanceWith n ''AdditiveMagma
      [ mkDecl '(+) [| (P.+) |] ]
  , mkInstance n ''Abelian
  , mkInstance n ''AdditiveSemigroup
  , mkInstanceWith n ''AdditiveMonoid
      [ mkDecl 'zero [| 0 |] ]
  , mkInstanceWith n ''DecidableZero
      [ mkDecl 'isZero [| (==0) |] ]
  ]

-- | Make abelean group instance of n, assuming Num n
mkAbelianGroupInstanceFromNum :: Name -> DecsQ
mkAbelianGroupInstanceFromNum n = concat <$> sequence 
  [ mkAbelianMonoidInstanceFromNum n
  , sequence
    [ mkInstanceWith n ''AdditiveGroup
        [ mkDecl '(-) [| (P.-) |]
        , mkDecl 'negate [| P.negate |]
        , mkDecl 'subtract [| P.subtract |]
        ]
    ]
  ]
