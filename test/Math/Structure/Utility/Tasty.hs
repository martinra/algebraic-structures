module Math.Structure.Utility.Tasty
where

import Control.Monad.Reader ( Reader, runReader
                            , ask
                            )
import Data.Functor.Identity
import Test.Tasty
import qualified Test.SmallCheck.Series as SCS
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC


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

testPropertyQC :: TestableProperty p => String -> p -> TestTree
testPropertyQC s p = testGroup "(QuickCheck)"
  [ QC.testProperty s p ]

testPropertySnC :: TestableProperty p => Int -> String -> p -> TestTree
testPropertySnC n s p = testGroup ("(SmallCheck with depth " ++ show n ++ ")")
  [ SC.testProperty s $ SC.changeDepth (const n) p ]

testPropertyQSC :: TestableProperty p => String -> p -> TestTree
testPropertyQSC s p = testGroup "(QuickCheck & SmallCheck)"
  [ QC.testProperty s p,
    SC.testProperty s p
  ]

testPropertyQSnC :: TestableProperty p => Int -> String -> p -> TestTree
testPropertyQSnC n s p = testGroup ("(QuickCheck & SmallCheck with depth " ++ show n ++ ")")
  [ QC.testProperty s p,
    SC.testProperty s$ SC.changeDepth (const n) p
  ]


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


runTestsQC :: [TestR [TestTree]] -> [TestTree]
runTestsQC = (`runTestR` testPropertyQC) . fmap concat . sequence 

runTestsSnC :: Int -> [TestR [TestTree]] -> [TestTree]
runTestsSnC n = (`runTestR` testPropertySnC n) . fmap concat . sequence 

runTestsQSC :: [TestR [TestTree]] -> [TestTree]
runTestsQSC = (`runTestR` testPropertyQSC) . fmap concat . sequence 

runTestsQSnC :: Int -> [TestR [TestTree]] -> [TestTree]
runTestsQSnC n = (`runTestR` testPropertyQSnC n) . fmap concat . sequence 
