{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}
module Math.Structure.Tasty.Unit.SmallCheck
where

import Data.Functor.Identity
import Test.SmallCheck.Series

import Math.Structure.Multiplicative


instance    (Monad m, DecidableUnit a, Serial Identity a)
         => Serial m (Unit a) where
  series = generate $ \d ->
             map Unit . filter isUnit $
             list d (series::Series Identity a) 
