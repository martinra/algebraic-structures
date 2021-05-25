module Math.Structure.Tasty.NonZero.QuickCheck
where

import Control.Applicative ( (<$>) )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Math.Structure.Additive


instance    (DecidableZero a, Arbitrary a)
         => Arbitrary (NonZero a) where
  arbitrary = nonZero <$> arbitrary `suchThat` (not . isZero)

  shrink = map nonZero . filter (not . isZero) .
           shrink . fromNonZero
