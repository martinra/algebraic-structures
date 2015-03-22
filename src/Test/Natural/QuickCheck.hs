module Test.Natural.QuickCheck
where

import Control.Applicative ( (<$>) )
import Numeric.Natural ( Natural )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


instance Arbitrary Natural where
  arbitrary = fmap fromInteger $ arbitrary `suchThat` (>=0)

  shrink = map fromIntegral . filter (>=0) . shrinkIntegral . toInteger
