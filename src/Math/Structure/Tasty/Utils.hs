{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  #-}

module Math.Structure.Tasty.Utils
where

import Test.Tasty
import qualified Test.SmallCheck.Series as SCS
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC


testProperty s p = testGroup "(QuickCheck & SmallCheck)"
  [ QC.testProperty s p
  , SC.testProperty s p
  ]

type Testable a = ( QC.Arbitrary a, SCS.Serial IO a
                  , Show a, Eq a )
