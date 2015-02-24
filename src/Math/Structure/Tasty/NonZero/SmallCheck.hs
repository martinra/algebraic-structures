{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}
module Math.Structure.Tasty.NonZero.SmallCheck
where

import Data.Functor.Identity
import Test.SmallCheck.Series

import Math.Structure.Additive


instance    (Monad m, DecidableZero a, Serial Identity a)
         => Serial m (NonZero a) where
  series = generate $ \d ->
             map nonZero . filter (not . isZero) $
             list d (series::Series Identity a) 
