{-# LANGUAGE OverloadedStrings #-}

module Data.EXPRESS.Parsers (
  pExpress
) where

import Prelude hiding (takeWhile)

import Control.Applicative (optional)
import Data.Attoparsec.ByteString
import Data.Either
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Data.EXPRESS.Schema

{- All lists in this AST are non-empty. When we need a list that can possibly
 - be empty, we wrap it into Maybe and signal its emptinness with Nothing.
 -
 - But parsers return simple lists, no matter if they are empty or not. This
 - function wraps the list into Maybe according to the rules mentioned above.
 - -}
listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs



-- schema_decl { schema_decl } .
pExpress :: Parser Express
pExpress = do
  schemas <- many1 pSchema
  skipWhitespace
  endOfInput <?> "unexpected end of file"
  return $ Express schemas

isWhitespace :: Word8 -> Bool
isWhitespace x = x == 0x20 -- space
              || x == 0x09 -- tab
              || x == 0x0A -- LF
              || x == 0x0D -- CR

-- ^ Skip zero or more whitespace characters
skipWhitespace :: Parser ()
skipWhitespace = skipWhile isWhitespace

-- ^ Skip one or more whitespace characters
skipWhitespace1 :: Parser ()
skipWhitespace1 = do
  skip isWhitespace
  skipWhitespace

-- SCHEMA schema_id [ schema_version_id ] ' ; ' schema_body END_SCHEMA ' ; ' .
pSchema :: Parser Schema
pSchema = do
  string "SCHEMA" <?> "SCHEMA keyword"
  skipWhitespace1
  id <- pSchemaId
  skipWhitespace
  versionId <- optional pSchemaVersionId
  skipWhitespace
  string ";" <?> "semicolon after schema id"
  skipWhitespace
  body <- pSchemaBody
  string "END_SCHEMA" <?> "END_SCHEMA keyword"
  skipWhitespace
  string ";"
  return $ Schema id versionId body

-- simple_id .
pSchemaId :: Parser SchemaId
pSchemaId = pSimpleId

-- string_literal .
pSchemaVersionId :: Parser SchemaVersionId
pSchemaVersionId = pStringLiteral

-- { interface_specification } [ constant_decl ] { declaration | rule_decl } .
pSchemaBody :: Parser SchemaBody
pSchemaBody = do
  skipWhitespace
  interfaces' <- many' pInterfaceSpecification
  let interfaces = listToMaybe interfaces'
--   constants <- optional pConstantDecl
--   other <- many' (eitherP pDeclaration pRuleDecl)
--   let declarations = listToMaybe $ lefts other
--   let rules = listToMaybe $ rights other
--   return $ SchemaBody interfaces constants declarations rules
  skipWhitespace
  return $ SchemaBody interfaces

-- letter { letter | digit | ' _ ' } .
pSimpleId :: Parser SimpleId
pSimpleId = do
  start <- takeWhile isLetter
  if BS.null start
    then return $ T.empty
    else do
      rest <- takeWhile (\x -> isLetter x || isDigit x || isUnderscore x)
      return $ TE.decodeUtf8With (TEE.replace '*') (start `BS.append` rest)

  where
  isUnderscore :: Word8 -> Bool
  isUnderscore ch = ch == 0x5F

isLetter, isDigit :: Word8 -> Bool
isLetter ch = (ch >= 0x41 && ch <= 0x5A) -- capital letters
           || (ch >= 0x61 && ch <= 0x7A) -- small letters

isDigit ch = ch >= 0x30 && ch <= 0x39

{- \q { ( \q \q ) | not_quote | \s | \x9 | \xA | \xD } \q .

     or

--    ' " ' encoded_character { encoded_character } ' " ' . -}
pStringLiteral :: Parser StringLiteral
pStringLiteral = choice [pSimpleStringLiteral, pEncodedStringLiteral]
  where
  -- \q { ( \q \q ) | not_quote | \s | \x9 | \xA | \xD } \q .
  pSimpleStringLiteral :: Parser T.Text
  pSimpleStringLiteral = do
    word8 apostrophe
    literal <- many' $ choice [pQQ, pNotQuote, pWhitespace]
    word8 apostrophe
    return $ TE.decodeUtf8 $ BS.concat literal

    where
    apostrophe :: Word8
    apostrophe = 0x27

    pQQ :: Parser BS.ByteString
    pQQ = do
      word8 apostrophe
      word8 apostrophe
      return $ BS.pack [apostrophe, apostrophe]

    pNotQuote :: Parser BS.ByteString
    pNotQuote = takeWhile1 okay
      where
      okay x = (isLetter x)
            || (isDigit x)
            -- ! " # $ % &
            || (x >= 0x21 && x <= 0x26)
            -- ( ) * + , - . /
            || (x >= 0x28 && x <= 0x2F)
            -- : ; < = > ? @
            || (x >= 0x3A && x <= 0x40)
            -- [ \ ] ^ _ `
            || (x >= 0x5B && x <= 0x60)
            -- { | } ~
            || (x >= 0x7B && x <= 0x7E)

    -- \s | \x9 | \xA | \xD
    pWhitespace :: Parser BS.ByteString
    pWhitespace = do
      ch <- satisfy isWhitespace
      return $ BS.singleton ch

  -- ' " ' encoded_character { encoded_character } ' " ' .
  pEncodedStringLiteral :: Parser T.Text
  pEncodedStringLiteral = do
    word8 quote
    str <- many1 pEncodedCharacter
    word8 quote
    return $ T.concat str
    where
    quote :: Word8
    quote = 0x34

    {-
    encoded_character = octet octet octet octet .
    octet = hex_digit hex_digit .
    hex_digit = digit | ' a ' | ' b ' | ' c ' | ' d ' | ' e ' | ' f ' .
    -}
    pEncodedCharacter :: Parser T.Text
    pEncodedCharacter = do
      octets <- count 4 pOctet
      return $ TE.decodeUtf32BE $ BS.pack octets

    pOctet :: Parser Word8
    pOctet = do
      high <- pHexDigit
      low  <- pHexDigit
      return $ (wordToNum high) * 16 + (wordToNum low)
      where
      wordToNum x = if (x >= 0x30 && x <= 0x39)
                      then x - 0x30 -- digit
                      else x - 0x61 -- letter

    pHexDigit :: Parser Word8
    pHexDigit = satisfy (\x -> (isDigit x) || (x >= 0x61 && x <= 0x66)) <?> "hex digit"

pInterfaceSpecification :: Parser InterfaceSpecification
pInterfaceSpecification = choice [pReference, pUse]
  where
  -- REFERENCE FROM schema_ref [ ' ( ' resource_or_rename { ' , ' resource_or_rename } ' ) ' ] ' ; ' .
  pReference = do
    skipWhitespace
    string "REFERENCE"
    skipWhitespace1
    string "FROM"
    skipWhitespace1
    ref <- pSchemaRef
    resources <- optional $ do
      skipWhitespace
      string "("
      skipWhitespace
      res1 <- pResourceOrRename
      rest <- many' $ do
        skipWhitespace
        string ","
        skipWhitespace
        pResourceOrRename
      skipWhitespace
      string ")"
      return $ res1 : rest
    skipWhitespace
    string ";"
    skipWhitespace
    return $ ReferenceClause ref resources

  -- USE FROM schema_ref [ ' ( ' named_type_or_rename { ' , ' named_type_or_rename } ' ) ' ] ' ; ' .
  pUse = do
    skipWhitespace
    string "USE"
    skipWhitespace1
    string "FROM"
    skipWhitespace1
    ref <- pSchemaRef
    renames <- optional $ do
      skipWhitespace
      string "("
      skipWhitespace
      rename1 <- pNamedTypeOrRename
      rest <- many' $ do
        skipWhitespace
        string ","
        skipWhitespace
        pNamedTypeOrRename
      skipWhitespace
      string ")"
      return $ rename1 : rest
    skipWhitespace
    string ";"
    skipWhitespace
    return $ UseClause ref renames

-- schema_id .
pSchemaRef :: Parser SchemaRef
pSchemaRef = pSchemaId

-- resource_ref [ AS rename_id ] .
pResourceOrRename :: Parser ResourceOrRename
pResourceOrRename = do
  ref <- pResourceRef
  as_id <- optional $ do
    skipWhitespace1
    string "AS"
    skipWhitespace1
    pRenameId
  return $ ResourceOrRename ref as_id

-- constant_ref | entity_ref | function_ref | procedure_ref | type_ref .
--
-- Since all refs are mapped into ids, which in turn are equivalent to
-- simple_id, we just skip to the end.
pResourceRef :: Parser ResourceRef
pResourceRef = pSimpleId

-- constant_id | entity_id | function_id | procedure_id | type_id .
--
-- In the end, all the IDs are SimpleId, so let's just skip the intermediate
-- step.
pRenameId :: Parser RenameId
pRenameId = pSimpleId

-- named_types [ AS ( entity_id | type_id ) ] .
pNamedTypeOrRename :: Parser NamedTypeOrRename
pNamedTypeOrRename = do
  skipWhitespace
  ref <- pNamedTypes
  as_id <- optional $ do
    skipWhitespace1
    string "AS"
    skipWhitespace1
    eitherP pEntityId pTypeId
  skipWhitespace
  return $ NamedTypeOrRename ref as_id

-- entity_ref | type_ref .
--
-- In the end, it's SimpleId.
pNamedTypes :: Parser NamedTypes
pNamedTypes = pSimpleId

-- simple_id .
pEntityId :: Parser EntityId
pEntityId = pSimpleId

-- simple_id .
pTypeId :: Parser TypeId
pTypeId = pSimpleId
