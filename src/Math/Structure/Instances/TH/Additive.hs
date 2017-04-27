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
import Math.Structure.Utility.TH


-- | Make abelean monoid instance of n, assuming Num n
mkAbelianMonoidInstanceFromNum :: CxtQ -> TypeQ -> DecsQ
mkAbelianMonoidInstanceFromNum cxt t = sequence
  [ mkInstanceWith cxt t [t|AdditiveMagma|]
      [ mkDecl '(+) [| (P.+) |] ]
  , mkInstance cxt t [t|Abelian|]
  , mkInstance cxt t [t|AdditiveSemigroup|]
  , mkInstanceWith cxt t [t|AdditiveMonoid|]
      [ mkDecl 'zero [| 0 |] ]
  , mkInstanceWith cxt t [t|DecidableZero|]
      [ mkDecl 'isZero [| (==0) |] ]
  ]

-- | Make abelean group instance of n, assuming Num n
mkAbelianGroupInstanceFromNum :: CxtQ -> TypeQ -> DecsQ
mkAbelianGroupInstanceFromNum cxt t = concat <$> sequence 
  [ mkAbelianMonoidInstanceFromNum cxt t
  , sequence
    [ mkInstanceWith cxt t [t|AdditiveGroup|]
        [ mkDecl '(-) [| (P.-) |]
        , mkDecl 'negate [| P.negate |]
        , mkDecl 'subtract [| P.subtract |]
        ]
    ]
  ]
