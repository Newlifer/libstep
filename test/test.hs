import Test.Hspec.Attoparsec
import Test.Tasty
import Test.Tasty.Hspec

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import System.IO (withFile, IOMode(ReadMode))

import Data.STEP.Parsers
import Data.EXPRESS.Schema
import Data.EXPRESS.Parsers

main :: IO ()
main = do
  stepParsersSpecs <- createStepParsersSpecs
  expressParsersSpecs <- createExpressParsersSpecs
  let tests = testGroup "Tests" [stepParsersSpecs, expressParsersSpecs]
  defaultMain tests

createStepParsersSpecs = testSpec "Parsing STEP" $ parallel $
  describe "success cases" $ do
    it "should parse case #1" $
      (C8.pack "(  1.45  , 666.    ,2.   ,6.022E23)") ~> parseStep
        `shouldParse` (Vector [1.45, 666.0, 2.0, 6.022e23])

    it "should parse case #2" $
      (C8.pack "(1.0,2.0,3.0,4.0)") ~> parseStep
        `shouldParse` (Vector [1.0, 2.0, 3.0, 4.0])

createExpressParsersSpecs = testSpec "Parsing EXPRESS" $ parallel $
  describe "success cases" $ do
    it "should parse empty schema" $
      withFile "test/data/empty.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> pExpress)
          (Express [Schema (T.pack "design") Nothing SchemaBody])

    it "should parse schema with version id" $
      withFile "test/data/version_id.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> pExpress)
          (Express [
            Schema
              (T.pack "design")
              (Just $ T.pack "{ISO standard 10303 part(41) object(1)\n\tversion(9)}")
              SchemaBody])
