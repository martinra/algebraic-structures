{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Instances.TH.Additive
where

import Prelude hiding ( (+), (-), negate, subtract )
import qualified Prelude as P

import Language.Haskell.TH

import Math.Structure.Additive
import Math.Structure.Utils.TH


-- | Make abelean group instance of n, assuming Num n
mkAbeleanGroupInstanceFromNum :: Name -> DecsQ
mkAbeleanGroupInstanceFromNum n = sequence
  [ mkInstanceWith n ''AdditiveMagma
      [ mkDecl '(+) [| (P.+) |] ]
  , mkInstance n ''Abelean
  , mkInstance n ''AdditiveSemigroup 
  , mkInstanceWith n ''AdditiveMonoid
      [ mkDecl 'zero [| 0 |] ]
  , mkInstanceWith n ''DecidableZero
      [ mkDecl 'isZero [| (==0) |] ]
  , mkInstanceWith n ''AdditiveGroup
      [ mkDecl '(-) [| (P.-) |]
      , mkDecl 'negate [| P.negate |]
      , mkDecl 'subtract [| P.subtract |]
      ]
  ]
