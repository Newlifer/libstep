{-# LANGUAGE OverloadedStrings #-}

module Data.EXPRESS.Parsers (
  pExpress
) where

import Prelude hiding (takeWhile)

import Control.Applicative (optional)
import Control.Monad (void, liftM)
import Data.Attoparsec.ByteString
import Data.Maybe (isJust)
import Data.Word (Word8())

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

between :: Parser a
        -> Parser b
        -> Parser d
        -> Parser d
between left right p = do
  void $ left
  result <- p
  void $ right
  return result

parens :: Parser a -> Parser a
parens =
  between
    (keyword "(")
    (keyword ")")

-- ^ A comma with optional whitespace on either side
commaSep :: Parser ()
commaSep = keyword ","

-- ^ "AS" keywords with non-optional whitespace on either side
asSep :: Parser ()
asSep = keyword1 "AS"

keyword :: BS.ByteString -> Parser ()
keyword k = (string k <?> "keyword") *> skipWhitespace

keyword1 :: BS.ByteString -> Parser ()
keyword1 k = (string k <?> "keyword") *> skipWhitespace1

lexeme :: Parser a -> Parser a
lexeme p = p <* skipWhitespace



-- schema_decl { schema_decl } .
pExpress :: Parser Express
pExpress = do
  skipWhitespace
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
pSchema =
  Schema <$>
    (keyword1 "SCHEMA" *> pSchemaId) <*>
    (optional pSchemaVersionId <* keyword ";") <*>
    (pSchemaBody <* (keyword "END_SCHEMA" *> keyword ";"))

-- simple_id .
pSchemaId :: Parser SchemaId
pSchemaId = lexeme pSimpleId

-- string_literal .
pSchemaVersionId :: Parser SchemaVersionId
pSchemaVersionId = lexeme pStringLiteral

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
      return $ TE.decodeUtf8 (start `BS.append` rest)

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
    void $ word8 apostrophe
    literal <- many' $ choice [pQQ, pNotQuote, pWhitespace]
    void $ word8 apostrophe
    return $ TE.decodeUtf8 $ BS.concat literal

    where
    apostrophe :: Word8
    apostrophe = 0x27

    pQQ :: Parser BS.ByteString
    pQQ = do
      void $ word8 apostrophe
      void $ word8 apostrophe
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
    void $ word8 quote
    str <- many1 pEncodedCharacter
    void $ word8 quote
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
    keyword1 "REFERENCE"
    keyword1 "FROM"
    ref <- pSchemaRef
    res <- optional $ parens $ (lexeme pResourceOrRename) `sepBy1` commaSep
    keyword ";"
    return $ ReferenceClause ref res

  -- USE FROM schema_ref [ ' ( ' named_type_or_rename { ' , ' named_type_or_rename } ' ) ' ] ' ; ' .
  pUse = do
    keyword1 "USE"
    keyword1 "FROM"
    ref <- pSchemaRef
    names <- optional $ parens $ (lexeme pNamedTypeOrRename) `sepBy1` commaSep
    keyword ";"
    return $ UseClause ref names

-- schema_id .
pSchemaRef :: Parser SchemaRef
pSchemaRef = lexeme pSchemaId

-- resource_ref [ AS rename_id ] .
pResourceOrRename :: Parser ResourceOrRename
pResourceOrRename =
  ResourceOrRename <$>
    (lexeme pResourceRef) <*>
    (optional $ asSep *> pRenameId)

-- constant_ref | entity_ref | function_ref | procedure_ref | type_ref .
--
-- Since all refs are mapped into ids, which in turn are equivalent to
-- simple_id, we just skip to the end.
pResourceRef :: Parser ResourceRef
pResourceRef = lexeme pSimpleId

-- constant_id | entity_id | function_id | procedure_id | type_id .
--
-- In the end, all the IDs are SimpleId, so let's just skip the intermediate
-- step.
pRenameId :: Parser RenameId
pRenameId = lexeme pSimpleId

-- named_types [ AS ( entity_id | type_id ) ] .
pNamedTypeOrRename :: Parser NamedTypeOrRename
pNamedTypeOrRename =
  NamedTypeOrRename <$>
    (lexeme pNamedTypes) <*>
    (optional $ asSep *> (lexeme $ eitherP pEntityId pTypeId))

-- entity_ref | type_ref .
--
-- In the end, it's SimpleId.
pNamedTypes :: Parser NamedTypes
pNamedTypes = lexeme pSimpleId

-- simple_id .
pEntityId :: Parser EntityId
pEntityId = lexeme pSimpleId

-- simple_id .
pTypeId :: Parser TypeId
pTypeId = lexeme pSimpleId
