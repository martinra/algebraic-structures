module Test.Natural.QuickCheck
where

import Control.Applicative ( (<$>) )
import Numeric.Natural ( Natural )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


instance Arbitrary Natural where
  arbitrary = sized $ \n -> fromInteger <$> choose (0, toInteger n)

  shrink x = fmap fromInteger $
             takeWhile (>0) (0:[ x'-i | i <- tail $ iterate (`quot` 2) x' ])
    where
    x' = toInteger x
