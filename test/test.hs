import Test.Hspec.Attoparsec
import Test.Tasty
import Test.Tasty.Hspec

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C8

import Data.STEP.Parsers

main :: IO ()
main = do
  specs <- createSpecs
  let tests = testGroup "Tests" [specs]
  defaultMain tests

createSpecs = testSpec "Parsing" $ parallel $
  describe "success cases" $ do
    it "should parse case1" $
      (C8.pack "(  1.45  , 666.    ,2.   ,6.022E23)") ~> parseStep
        `shouldParse` (Vector [1.45, 666.0, 2.0, 6.022e23])

    it "should parse case2" $
      (C8.pack "(1.0,2.0,3.0,4.0)") ~> parseStep
        `shouldParse` (Vector [1.0, 2.0, 3.0, 4.0])
