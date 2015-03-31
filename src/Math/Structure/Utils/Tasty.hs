{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , RankNTypes
  #-}

module Math.Structure.Utils.Tasty
where

import Control.Monad.Reader ( Reader, runReader
                            , ask
                            )
import Data.Functor.Identity
import Test.Tasty
import qualified Test.SmallCheck.Series as SCS
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC


newtype TestPropertyFunc =
  TestPropertyFunc ( forall p .    TestableProperty p
                                => String -> p -> TestTree )

type TestR a = Reader TestPropertyFunc a

runTestR :: TestR a
         -> ( forall p .    TestableProperty p
                         => String -> p -> TestTree )
         -> a
runTestR t f = runReader t (TestPropertyFunc f)

withTestProperty ::    ((forall p.    TestableProperty p
                                   => String -> p -> TestTree) -> a)
                    -> TestR a
withTestProperty f = ask >>= \(TestPropertyFunc testProperty) ->
                     return $ f testProperty


type Testable a = ( QC.Arbitrary a, SCS.Serial IO a
                  , Show a, Eq a )

class (QC.Testable p, SC.Testable IO p) => TestableProperty p

instance    TestableProperty Bool
instance    ( Testable a )
         => TestableProperty (a -> Bool)
instance    ( Testable a, Testable b )
         => TestableProperty (a -> b -> Bool)
instance    ( Testable a, Testable b, Testable c )
         => TestableProperty (a -> b -> c -> Bool)
instance    ( Testable a, Testable b, Testable c, Testable d )
         => TestableProperty (a -> b -> c -> d -> Bool)
