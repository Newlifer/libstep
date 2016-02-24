import Test.Hspec.Attoparsec
import Test.Tasty
import Test.Tasty.Hspec

import Data.Attoparsec.ByteString (endOfInput)
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
      (C8.pack "(  1.45  , 666.    ,2.   ,6.022E23)")
        ~> (parseStep <* endOfInput)
        `shouldParse` (Vector [1.45, 666.0, 2.0, 6.022e23])

    it "should parse case #2" $
      (C8.pack "(1.0,2.0,3.0,4.0)") ~> (parseStep <* endOfInput)
        `shouldParse` (Vector [1.0, 2.0, 3.0, 4.0])

createExpressParsersSpecs :: IO TestTree
createExpressParsersSpecs = testSpec "Parsing EXPRESS" $ parallel $
  describe "success cases" $ do
    it "should parse empty schema" $
      withFile "test/data/empty.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> (pExpress <* endOfInput))
          (Express [
            Schema
              (T.pack "design")
              Nothing
              (SchemaBody Nothing Nothing)])

    it "should parse schema with version id" $
      withFile "test/data/version_id.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> (pExpress <* endOfInput))
          (Express [
            Schema
              (T.pack "design")
              (Just $ T.pack "{ISO standard 10303 part(41) object(1)\
                             \\n\tversion(9)}")
              (SchemaBody Nothing Nothing)])

    it "should parse USE clause" $
      withFile "test/data/use_clause.exp" ReadMode $ \h -> do
        schema <- C8.hGetContents h
        shouldParse
          (schema ~> (pExpress <* endOfInput))
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
          (schema ~> (pExpress <* endOfInput))
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
        (C8.pack "1" ~> (pInteger <* endOfInput))
        (1 :: Integer)

    it "should parse digit sequences" $
      shouldParse
        (C8.pack "1234567890" ~> (pInteger <* endOfInput))
        (1234567890 :: Integer)

    it "should parse simple string literals" $
      shouldParse
        (C8.pack "'It''s just a test'" ~> (pStringLiteral <* endOfInput))
        (T.pack "It's just a test")

    it "should parse encoded string literals" $
      (C8.pack "\"00000061000000620000003f00000061\""
        ~> (pStringLiteral <* endOfInput))
        `shouldParse` (T.pack "ab?a")

    it "should parse simple `CONSTANT' block" $
      (C8.pack "CONSTANT a : BOOLEAN := TRUE; END_CONSTANT;"
        ~> (pConstantDecl <* endOfInput))
        `shouldParse` (ConstantDecl [
          ConstantBody
            (T.pack "a")
            BooleanType
            (ELiteral $ LLogicalLiteral TRUE)])

    it "should parse 2+2" $
      shouldParse
        (C8.pack "2+2" ~> (pExpression <* endOfInput))
        (Add
          (ELiteral (IntegerLiteral 2))
          (ELiteral (IntegerLiteral 2)))

    it "should parse 2*2" $
      shouldParse
        (C8.pack "2*2" ~> (pExpression <* endOfInput))
        (Multiply
          (ELiteral (IntegerLiteral 2))
          (ELiteral (IntegerLiteral 2)))

    it "should parse 2*2+2/2-5" $
      shouldParse
        (C8.pack "2*2+2/2-5" ~> (pExpression <* endOfInput))
        (Subtract
          (Add
            (Multiply
              (ELiteral (IntegerLiteral 2))
              (ELiteral (IntegerLiteral 2)))
            (Divide
              (ELiteral (IntegerLiteral 2))
              (ELiteral (IntegerLiteral 2))))
          (ELiteral (IntegerLiteral 5)))

    it "should parse -10**2" $
      shouldParse
        (C8.pack "-10**2" ~> (pExpression <* endOfInput))
        (Pow
          (Negate $ ELiteral $ IntegerLiteral 10)
          (ELiteral $ IntegerLiteral 2))

    it "should parse 10/20*30" $
      shouldParse
        (C8.pack "10/20*30" ~> (pExpression <* endOfInput))
        (Multiply
          (Divide
            (ELiteral $ IntegerLiteral 10)
            (ELiteral $ IntegerLiteral 20))
          (ELiteral $ IntegerLiteral 30))

    it "should parse (10+20) DIV 3" $
      shouldParse
        (C8.pack "(10+20) DIV 3" ~> (pExpression <* endOfInput))
        (Div
          (Add
            (ELiteral $ IntegerLiteral 10)
            (ELiteral $ IntegerLiteral 20))
          (ELiteral $ IntegerLiteral 3))

    it "should parse empty aggregate initializer" $
      shouldParse
        (C8.pack "[]" ~> (pExpression <* endOfInput))
        (AggregateInitializer Nothing)

    it "should parse non-empty aggregate initializer" $
      shouldParse
        (C8.pack "[1, 3, 6, 9*8, -12]" ~> (pExpression <* endOfInput))
        (AggregateInitializer $ Just
          [ Element (ELiteral (IntegerLiteral 1)) Nothing
          , Element (ELiteral (IntegerLiteral 3)) Nothing
          , Element (ELiteral (IntegerLiteral 6)) Nothing
          , Element
              (Multiply
                (ELiteral (IntegerLiteral 9))
                (ELiteral (IntegerLiteral 8)))
              Nothing
          , Element (Negate (ELiteral (IntegerLiteral 12))) Nothing])

    it "should parse aggregate initializer with repetitions" $
      shouldParse
        (C8.pack "[ TRUE : 5]" ~> (pExpression <* endOfInput))
        (AggregateInitializer
          (Just [
            Element
              (ELiteral (LLogicalLiteral TRUE))
              (Just $ ELiteral $ IntegerLiteral 5)]))
