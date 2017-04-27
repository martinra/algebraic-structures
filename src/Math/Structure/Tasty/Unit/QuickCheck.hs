module Math.Structure.Tasty.Unit.QuickCheck
where

import Control.Applicative ( (<$>) )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Math.Structure.Multiplicative


instance    (DecidableUnit a, Arbitrary a)
         => Arbitrary (Unit a) where
  arbitrary = Unit <$> arbitrary `suchThat` isUnit

  shrink = map Unit . filter isUnit . shrink . fromUnit
