{-# LANGUAGE
    TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Math.Structure.Instances.Standard.Additive
where

import Prelude hiding ( (+), (-), negate, subtract )
import qualified Prelude as P
import Numeric.Natural ( Natural )

import Math.Structure.Instances.TH.Additive


mkAbelianGroupInstanceFromNum ''Integer
mkAbelianGroupInstanceFromNum ''Int
mkAbelianMonoidInstanceFromNum ''Natural

mkAbelianGroupInstanceFromNum ''Rational

