import Test.Hspec.Attoparsec
import Test.Tasty
import Test.Tasty.Hspec

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import System.IO (withFile, IOMode(ReadMode))

import Data.STEP.Parsers
import Data.EXPRESS.Schema
import Data.EXPRESS.Internal.Parsers

main :: IO ()
main = do
  stepParsersSpecs <- createStepParsersSpecs
  expressParsersSpecs <- createExpressParsersSpecs
  let tests = testGroup "Tests" [stepParsersSpecs, expressParsersSpecs]
  defaultMain tests

createStepParsersSpecs :: IO TestTree
createStepParsersSpecs = testSpec "Parsing STEP" $ parallel $
  describe "success cases" $ do
    it "should parse case #1" $
      (C8.pack "(  1.45  , 666.    ,2.   ,6.022E23)") ~> parseStep
        `shouldParse` (Vector [1.45, 666.0, 2.0, 6.022e23])

    it "should parse case #2" $
      (C8.pack "(1.0,2.0,3.0,4.0)") ~> parseStep
        `shouldParse` (Vector [1.0, 2.0, 3.0, 4.0])

createExpressParsersSpecs :: IO TestTree
createExpressParsersSpecs = testSpec "Parsing EXPRESS" $ parallel $
  describe "success cases" $ do
    it "should parse empty schema" $
      withFile "test/data/empty.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> pExpress)
          (Express [
            Schema
              (T.pack "design")
              Nothing
              (SchemaBody Nothing Nothing)])

    it "should parse schema with version id" $
      withFile "test/data/version_id.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> pExpress)
          (Express [
            Schema
              (T.pack "design")
              (Just $ T.pack "{ISO standard 10303 part(41) object(1)\n\tversion(9)}")
              (SchemaBody Nothing Nothing)])

    it "should parse USE clause" $
      withFile "test/data/use_clause.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> pExpress)
          (Express [
            Schema
              (T.pack "s1")
              Nothing
              (SchemaBody
                (Just [
                  UseClause
                    (T.pack "s2")
                    (Just [
                      NamedTypeOrRename
                        (T.pack "e1")
                        (Just $ T.pack "e2")])])
                Nothing)])

    it "should parse REFERENCE clause" $
      withFile "test/data/reference_clause.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> pExpress)
          (Express [
            Schema
              (T.pack "s2")
              Nothing
              (SchemaBody
                (Just [
                  ReferenceClause
                    (T.pack "s1")
                    (Just [
                      ResourceOrRename
                        (T.pack "e2")
                        (Just $ T.pack "e20")])])
                Nothing)])

    it "should parse single digits" $
      shouldParse
        (C8.pack "1" ~> pInteger)
        (1 :: Integer)

    it "should parse digit sequences" $
      shouldParse
        (C8.pack "1234567890" ~> pInteger)
        (1234567890 :: Integer)

    it "should parse simple string literals" $
      shouldParse
        (C8.pack "'It''s just a test'" ~> pStringLiteral)
        (T.pack "It's just a test")

    it "should parse encoded string literals" $
      shouldParse
        (C8.pack "\"00000061000000620000003f00000061\"" ~> pStringLiteral)
        (T.pack "ab?a")
