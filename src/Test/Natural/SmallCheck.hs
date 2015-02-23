{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Test.Natural.SmallCheck
where

import Numeric.Natural ( Natural )
import Test.SmallCheck.Series


instance Monad m => Serial m Natural where
  series = generate $ \d -> map fromIntegral [0..d]
