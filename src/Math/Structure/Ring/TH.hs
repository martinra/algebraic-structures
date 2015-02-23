{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Ring.TH
where

import Control.Monad ( sequence )
import Language.Haskell.TH

import Math.Structure.Ring
import Math.Structure.Utils.TH


mkRingInstance :: Name -> DecsQ
mkRingInstance r = sequence
  [ mkInstance r ''Ring
  , mkInstance r ''Rng
  , mkInstance r ''Rig
  , mkInstance r ''Semiring
  , mkInstance r ''Distributive
  ]

