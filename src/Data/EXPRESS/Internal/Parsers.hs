{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Data.EXPRESS.Internal.Parsers where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>), optional)
import Control.Monad (void, liftM)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Scientific (Scientific(), scientific)
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

digitsToInteger :: [Int] -> Integer
digitsToInteger [] = error "digitsToInteger got called on empty list"
digitsToInteger [i] = fromIntegral i
digitsToInteger (start:rest) =
  foldl'
    (\acc i -> acc * 10 + fromIntegral i)
    (fromIntegral start)
    rest

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
keyword k = do
  _ <- string k <?> (T.unpack $ TE.decodeUtf8 k) ++ " expected"
  skipWhitespace

keyword1 :: BS.ByteString -> Parser ()
keyword1 k = do
  _ <- string k <?> (T.unpack $ TE.decodeUtf8 k) ++ " expected"
  -- ensure that the keyword is not followed by a letter or a digit
  next <- peekWord8
  case next of
    Nothing -> skipWhitespace
    Just x  ->
      if inClass ";, \n\r\t" x
        then skipWhitespace
        else fail "any of `;, \\n\\r\\t' expected after keyword"

lexeme :: Parser a -> Parser a
lexeme p = p <* skipWhitespace



-- schema_decl { schema_decl } .
pExpress :: Parser Express
pExpress = do
  skipWhitespace
  schemas <- many1 $ lexeme pSchema
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

-- SCHEMA schema_id [ schema_version_id ] ' ; ' schema_body END_SCHEMA ' ; ' .
pSchema :: Parser Schema
pSchema =
  Schema <$>
    (keyword1 "SCHEMA" *> (lexeme pSchemaId)) <*>
    (optional (lexeme pSchemaVersionId) <* keyword ";") <*>
    ((lexeme pSchemaBody) <* (keyword "END_SCHEMA" *> keyword ";"))

-- simple_id .
pSchemaId :: Parser SchemaId
pSchemaId = lexeme pSimpleId

-- string_literal .
pSchemaVersionId :: Parser SchemaVersionId
pSchemaVersionId = lexeme pStringLiteral

-- { interface_specification } [ constant_decl ] { declaration | rule_decl } .
pSchemaBody :: Parser SchemaBody
pSchemaBody = do
  interfaces' <- many' $ lexeme pInterfaceSpecification
  let interfaces = listToMaybe interfaces'
  constants <- optional $ lexeme pConstantDecl
--   other <- many' (eitherP pDeclaration pRuleDecl)
--   let declarations = listToMaybe $ lefts other
--   let rules = listToMaybe $ rights other
--   return $ SchemaBody interfaces constants declarations rules
  return $ SchemaBody interfaces constants

-- letter { letter | digit | ' _ ' } .
pSimpleId :: Parser SimpleId
pSimpleId = do
  start <- takeWhile1 isLetter
  rest <- takeWhile (\x -> isLetter x || isDigit x || isUnderscore x)
  return $ TE.decodeUtf8 (start `BS.append` rest)

  where
  isUnderscore :: Word8 -> Bool
  isUnderscore ch = ch == 0x5F

isLetter, isDigit :: Word8 -> Bool
isLetter ch = (ch >= 0x41 && ch <= 0x5A) -- capital letters
           || (ch >= 0x61 && ch <= 0x7A) -- small letters

isDigit ch = ch >= 0x30 && ch <= 0x39

pDigit :: Parser Int
pDigit = (word8 0x30 *> pure 0)
     <|> (word8 0x31 *> pure 1)
     <|> (word8 0x32 *> pure 2)
     <|> (word8 0x33 *> pure 3)
     <|> (word8 0x34 *> pure 4)
     <|> (word8 0x35 *> pure 5)
     <|> (word8 0x36 *> pure 6)
     <|> (word8 0x37 *> pure 7)
     <|> (word8 0x38 *> pure 8)
     <|> (word8 0x39 *> pure 9)

pInteger :: Parser Integer
pInteger = digitsToInteger <$> many1 pDigit

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
      return $ BS.singleton apostrophe

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
    quote = 0x22

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
                      then x - 0x30      -- digit
                      else 10 + x - 0x61 -- letter

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
    ref <- lexeme pSchemaRef
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
    (optional $ asSep *> (lexeme pSimpleId))

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

-- CONSTANT constant_body { constant_body } END_CONSTANT ' ; ' .
pConstantDecl :: Parser ConstantDecl
pConstantDecl = do
  keyword1 "CONSTANT"
  bodies <- many1 $ lexeme pConstantBody
  keyword "END_CONSTANT"
  keyword ";"
  return $ ConstantDecl bodies

-- constant_id ' : ' instantiable_type ' := ' expression ' ; ' .
pConstantBody :: Parser ConstantBody
pConstantBody = do
  ConstantBody <$>
    (lexeme pConstantId <* keyword ":") <*>
    (lexeme pInstantiableType <* keyword ":=") <*>
    (lexeme pExpression <* keyword ";")

-- simple_id .
pConstantId :: Parser ConstantId
pConstantId = lexeme pSimpleId

-- concrete_types | entity_ref .
pInstantiableType :: Parser EXPRESSType
pInstantiableType = lexeme $ choice [pConcreteTypes, pEntityRef]

-- aggregation_types | simple_types | type_ref .
pConcreteTypes :: Parser EXPRESSType
pConcreteTypes = lexeme $ choice [pAggregationTypes, pSimpleTypes, pTypeRef]

-- array_type | bag_type | list_type | set_type .
pAggregationTypes :: Parser EXPRESSType
pAggregationTypes = lexeme $ choice [pArrayType, pBagType, pListType, pSetType]

-- entity_id .
pEntityRef :: Parser EXPRESSType
pEntityRef = EntityRef <$> lexeme pEntityId

-- | Parse an arithmetic expression.
--
-- For this parser, we stopped following WSN, because it doesn't even specify
-- how "1+2+3" should be parsed. Instead, we're using Data.Attoparsec.Expr to
-- build a parser according to the textual description from the specification.
pExpression :: Parser Expression
pExpression = buildExpressionParser table pTerm
  where
  pTerm = -- pSimpleId -- ссылка
          -- pSimpleId '.' pSimpleId -- префиксная ссылка или ссылка на атрибут
                                     -- (неразличимы на этапе парсинга)
          -- pSimpleId '\' pSimpleId -- групповая ссылка
          -- TODO: implement the above once we grasped the spec better
          keyword "(" *> (lexeme pExpression) <* keyword ")"
      <|> ELiteral <$> (lexeme pLiteral)
      <|> (lexeme pAggregateInitializer)
      <|> (lexeme pEntityConstructor)
      <|> (lexeme pEnumerationReference)
      <|> (lexeme pInterval)
      <|> (lexeme pQueryExpression)

  -- ' [ ' [ element { ' , ' element } ] ' ] ' .
  pAggregateInitializer = do
    keyword "["
    elements <- optional $ (lexeme pElement) `sepBy1` commaSep
    keyword "]"
    return $ AggregateInitializer elements

  -- entity_ref ' ( ' [ expression { ' , ' expression } ] ' ) ' .
  pEntityConstructor = do
    eref <- lexeme pEntityRef
    keyword "("
    exprs <- optional $ (lexeme pExpression) `sepBy1` commaSep
    keyword ")"
    return $ EntityConstructor eref exprs

  -- [ type_ref ' . ' ] enumeration_ref .
  pEnumerationReference =
    EnumerationReference <$>
      (optional $ lexeme pTypeRef) <*>
      (lexeme pEnumerationRef)

  -- ' { ' interval_low interval_op interval_item interval_op interval_high ' } ' .
  pInterval =
    Interval <$>
      (keyword "{" *> lexeme pIntervalLow) <*>
      (lexeme pIntervalOp) <*>
      (lexeme pIntervalItem) <*>
      (lexeme pIntervalOp) <*>
      (lexeme pIntervalHigh <* keyword "}")

  -- QUERY ' ( ' variable_id ' <* ' aggregate_source ' | ' logical_expression ' ) ' .
  pQueryExpression = do
    keyword1 "QUERY"
    keyword "("
    vid <- lexeme pVariableId
    keyword "<*"
    source <- pAggregateSource
    keyword "|"
    expr <- pLogicalExpression
    keyword ")"
    return $ QueryExpression vid source expr


  table = [ -- Technically, the highest priority is reserved for links: stuff
            -- like "a[b]", "a.b", and "a\b". But we incorporated those things
            -- in terms parser, so we don't need to handle them here

            [ Prefix (keyword   "+"    *> return id              )
            , Prefix (keyword   "-"    *> return Negate          )
            , Prefix (keyword1  "NOT"  *> return Not             )
            ]
          , [ Infix  (keyword   "**"   *> return Pow             ) AssocLeft
            ]
          , [ Infix  (keyword   "*"    *> return Multiply        ) AssocLeft
            , Infix  (keyword   "/"    *> return Divide          ) AssocLeft
            , Infix  (keyword1  "DIV"  *> return Div             ) AssocLeft
            , Infix  (keyword1  "MOD"  *> return Mod             ) AssocLeft
            , Infix  (keyword1  "AND"  *> return And             ) AssocLeft
            , Infix  (keyword   "||"   *> return Compose         ) AssocLeft
            ]
          , [ Infix  (keyword   "+"    *> return Add             ) AssocLeft
            , Infix  (keyword   "-"    *> return Subtract        ) AssocLeft
            , Infix  (keyword1  "OR"   *> return Or              ) AssocLeft
            , Infix  (keyword1  "XOR"  *> return Xor             ) AssocLeft
            ]
          , [ Infix  (keyword   "="    *> return Equals          ) AssocLeft
            , Infix  (keyword   "<>"   *> return NotEquals       ) AssocLeft
            , Infix  (keyword   "<="   *> return LessOrEquals    ) AssocLeft
            , Infix  (keyword   ">="   *> return GreaterOrEquals ) AssocLeft
            , Infix  (keyword   "<"    *> return Less            ) AssocLeft
            , Infix  (keyword   ">"    *> return Greater         ) AssocLeft
            , Infix  (keyword   ":=:"  *> return WeirdEquals     ) AssocLeft
            , Infix  (keyword   ":<>:" *> return WeirdNotEquals  ) AssocLeft
            , Infix  (keyword1  "IN"   *> return In              ) AssocLeft
            , Infix  (keyword1  "LIKE" *> return Like            ) AssocLeft
            ]
          ]

-- binary_type | boolean_type | integer_type | logical_type | number_type | real_type | string_type .
pSimpleTypes :: Parser EXPRESSType
pSimpleTypes =
  lexeme $ choice [
      -- BINARY [ width_spec ] .
      BinaryType <$> (keyword1 "BINARY" *> (optional $ lexeme pWidthSpec))

      -- BOOLEAN .
    , keyword1 "BOOLEAN" *> pure BooleanType

      -- INTEGER .
    , keyword1 "INTEGER" *> pure IntegerType

      -- LOGICAL .
    , keyword1 "LOGICAL" *> pure LogicalType

      -- NUMBER .
    , keyword1 "NUMBER" *> pure NumberType

      -- REAL [ ' ( ' precision_spec ' ) ' ] .
    , lexeme pRealType

      -- STRING [ width_spec ] .
    , StringType <$> (keyword1 "STRING" *> (optional $ lexeme pWidthSpec))
    ]
  where
  -- ' ( ' width ' ) ' [ FIXED ] .
  pWidthSpec :: Parser WidthSpec
  pWidthSpec =
    WidthSpec <$>
      (parens $ lexeme pWidth) <*>
      (liftM isJust $ optional $ keyword "FIXED")

  -- numeric_expression .
  -- ...that translates to...
  -- simple_expression .
  pWidth :: Parser Width
  pWidth = lexeme pExpression

  -- REAL [ ' ( ' precision_spec ' ) ' ] .
  pRealType :: Parser EXPRESSType
  pRealType = RealType <$>
    (keyword1 "REAL" *> (optional $ lexeme pNumericExpression))

-- type_id .
pTypeRef :: Parser EXPRESSType
pTypeRef = TypeRef <$> lexeme pTypeId

-- ARRAY bound_spec OF [ OPTIONAL ] [ UNIQUE ] instantiable_type .
pArrayType :: Parser EXPRESSType
pArrayType = do
  keyword1 "ARRAY"
  bounds <- lexeme pBoundSpec
  keyword1 "OF"
  opt <- liftM isJust $ optional $ keyword1 "OPTIONAL"
  uniq <- liftM isJust $ optional $ keyword1 "UNIQUE"
  t <- lexeme pInstantiableType
  return $ ArrayType bounds opt uniq t

-- BAG [ bound_spec ] OF instantiable_type .
pBagType :: Parser EXPRESSType
pBagType = do
  keyword1 "BAG"
  bounds <- optional $ lexeme pBoundSpec
  keyword1 "OF"
  t <- lexeme pInstantiableType
  return $ BagType bounds t

-- LIST [ bound_spec ] OF [ UNIQUE ] instantiable_type .
pListType :: Parser EXPRESSType
pListType = do
  keyword1 "LIST"
  bounds <- optional $ lexeme pBoundSpec
  keyword1 "OF"
  uniq <- liftM isJust $ optional $ keyword1 "UNIQUE"
  t <- lexeme pInstantiableType
  return $ ListType bounds uniq t

-- SET [ bound_spec ] OF instantiable_type .
pSetType :: Parser EXPRESSType
pSetType = do
  keyword1 "SET"
  bounds <- optional $ lexeme pBoundSpec
  keyword1 "OF"
  t <- lexeme pInstantiableType
  return $ SetType bounds t

-- ' [ ' bound_1 ' : ' bound_2 ' ] ' .
pBoundSpec :: Parser BoundSpec
pBoundSpec = do
  keyword "["
  b1 <- lexeme pBound1
  keyword ":"
  b2 <- lexeme pBound2
  keyword "]"
  return $ BoundSpec b1 b2

-- expression [ ' : ' repetition ] .
pElement :: Parser Element
pElement =
  Element <$>
    (lexeme pExpression) <*>
    (optional $ keyword ":" *> lexeme pRepetition)

-- enumeration_id .
pEnumerationRef :: Parser EnumerationRef
pEnumerationRef = lexeme pEnumerationId

-- simple_id .
pEnumerationId :: Parser EnumerationId
pEnumerationId = lexeme pSimpleId

-- simple_expression .
pIntervalLow :: Parser IntervalLow
pIntervalLow = lexeme pExpression

-- ' < ' | ' <= ' .
pIntervalOp :: Parser IntervalOp
pIntervalOp =
  lexeme $ choice [
      keyword "<"  *> pure IntervalLess
    , keyword "<=" *> pure IntervalLessEqual
    ]

-- simple_expression .
pIntervalItem :: Parser IntervalItem
pIntervalItem = lexeme pExpression

-- simple_expression .
pIntervalHigh :: Parser IntervalHigh
pIntervalHigh = lexeme pExpression

-- numeric_expression .
pBound1 :: Parser Bound1
pBound1 = lexeme pNumericExpression

-- numeric_expression .
pBound2 :: Parser Bound2
pBound2 = lexeme pNumericExpression

-- simple_id .
pVariableId :: Parser VariableId
pVariableId = lexeme pSimpleId

-- simple_expression .
pAggregateSource :: Parser AggregateSource
pAggregateSource = pExpression

-- expression .
pLogicalExpression :: Parser LogicalExpression
pLogicalExpression = pExpression

-- binary_literal | logical_literal | real_literal | string_literal .
pLiteral :: Parser Literal
pLiteral =
  lexeme $ choice [
      -- ' % ' bit { bit } .
      BinaryLiteral <$> (keyword "%" *> many1 (lexeme pBit))

      -- FALSE | TRUE | UNKNOWN .
    , LLogicalLiteral <$> (lexeme pLogicalLiteral)

      {-digits .
      ...that translates into...
      digit { digit } .
      ...which turns into...
      digit = ' 0 ' | ' 1 ' | ' 2 ' | ' 3 ' | ' 4 ' | ' 5 ' | ' 6 ' | ' 7 ' | ' 8 ' | ' 9 ' .-}
    , IntegerLiteral <$> (lexeme pInteger)

      -- digits ' . ' [ digits ] [ ' e ' [ sign ] digits ]
    , RealLiteral <$> (lexeme pScientific)

    , LStringLiteral <$> (lexeme pStringLiteral)
    ]

-- digits ' . ' [ digits ] [ ' e ' [ sign ] digits ]
pScientific :: Parser Scientific
pScientific = do
  c <- pInteger
  void $ string "."
  f <- many' pDigit
  e <- do
    void $ string "e"
    sign <- optional $ pSign
    e <- pInteger
    case sign of
      Just "-" -> return $ negate e
      Just "+" -> return          e
      Nothing  -> return          e
      _        -> error "pScientific: pSign returned something unexpected"
    <|> pure 0

  -- Data.Scientific.Scientific constructor expects two arguments: integer
  -- conefficient and integer exponent. But what we have at the moment is
  -- floating-point coefficient, represented as integral part `c` and
  -- floating-point part `f`. `c` is held in `Integer`, and `f` is just a list
  -- of digits (each represented by an `Int`).
  --
  -- We're going to:
  -- - let `p` be the number of digits in `f`;
  -- - let `f'` be `f` converted into `Integer`;
  -- - let `c'` be `c * 10^p + f'`;
  -- - let `e'` be `e + p`.
  --
  -- We can now use `c'` and `e'` as parameters for Scientific type constructor.
  let p  = fromIntegral $ length f
  let f' = digitsToInteger f
  let c' = c * 10^p + f'
  let e' = e + p

  -- Actually, we need to hop through one more loop. Data.Scientific.Scientific
  -- uses `Int` for exponent, but the one we just computed is `Integer`. Let's
  -- do a quick conversion.

  let e'' =
        if e' > fromIntegral (maxBound :: Int)
          then fromInteger e'
          else error "pScientific: can't represent parsed number as \
                     \Data.Scientific.Scientific"

  return $ scientific c' e''

  where
    pSign = string "-" <|> string "+"

-- numeric_expression .
pRepetition :: Parser NumericExpression
pRepetition = pNumericExpression

-- ' 0 ' | ' 1 ' .
pBit :: Parser Bit
pBit =
  lexeme $ choice [
      keyword "0" *> pure Zero
    , keyword "1" *> pure One
    ]

-- FALSE | TRUE | UNKNOWN .
pLogicalLiteral :: Parser LogicalLiteral
pLogicalLiteral =
  lexeme $ choice [
      keyword1 "FALSE"   *> pure FALSE
    , keyword1 "TRUE"    *> pure TRUE
    , keyword1 "UNKNOWN" *> pure UNKNOWN
    ]

-- CONST_E | PI | SELF | ' ? ' .
pBuiltInConstant :: Parser BuiltInConstant
pBuiltInConstant =
  lexeme $ choice [
      keyword1 "CONST_E" *> pure CONST_E
    , keyword1 "PI"      *> pure PI
    , keyword1 "SELF"    *> pure SELF
    , keyword1 "?"       *> pure QuestionMark
    ]

-- constant_id .
pConstantRef :: Parser ConstantRef
pConstantRef = lexeme pConstantId

-- ( built_in_function | function_ref ) [ actual_parameter_list ] .
pFunctionCall :: Parser FunctionCall
pFunctionCall =
  FunctionCall <$>
    (lexeme $ eitherP pBuiltInFunction pFunctionRef) <*>
    (optional $ lexeme pActualParameterList)

-- ABS | ACOS | ASIN | ATAN | BLENGTH | COS | EXISTS | EXP | FORMAT | HIBOUND | HIINDEX | LENGTH | LOBOUND | LOINDEX | LOG | LOG2 | LOG10 | NVL | ODD | ROLESOF | SIN | SIZEOF | SQRT | TAN | TYPEOF | USEDIN | VALUE | VALUE_IN | VALUE_UNIQUE .
pBuiltInFunction :: Parser BuiltInFunction
pBuiltInFunction =
      (keyword1 "ABS" *> pure ABS)
  <|> (keyword1 "ACOS" *> pure ACOS)
  <|> (keyword1 "ASIN" *> pure ASIN)
  <|> (keyword1 "ATAN" *> pure ATAN)
  <|> (keyword1 "BLENGTH" *> pure BLENGTH)
  <|> (keyword1 "COS" *> pure COS)
  <|> (keyword1 "EXISTS" *> pure EXISTS)
  <|> (keyword1 "EXP" *> pure EXP)
  <|> (keyword1 "FORMAT" *> pure FORMAT)
  <|> (keyword1 "HIBOUND" *> pure HIBOUND)
  <|> (keyword1 "HIINDEX" *> pure HIINDEX)
  <|> (keyword1 "LENGTH" *> pure LENGTH)
  <|> (keyword1 "LOBOUND" *> pure LOBOUND)
  <|> (keyword1 "LOINDEX" *> pure LOINDEX)
  <|> (keyword1 "LOG" *> pure LOG)
  <|> (keyword1 "LOG2" *> pure LOG2)
  <|> (keyword1 "LOG10" *> pure LOG10)
  <|> (keyword1 "NVL" *> pure NVL)
  <|> (keyword1 "ODD" *> pure ODD)
  <|> (keyword1 "ROLESOF" *> pure ROLESOF)
  <|> (keyword1 "SIN" *> pure SIN)
  <|> (keyword1 "SIZEOF" *> pure SIZEOF)
  <|> (keyword1 "SQRT" *> pure SQRT)
  <|> (keyword1 "TAN" *> pure TAN)
  <|> (keyword1 "TYPEOF" *> pure TYPEOF)
  <|> (keyword1 "USEDIN" *> pure USEDIN)
  <|> (keyword1 "VALUE" *> pure VALUE)
  <|> (keyword1 "VALUE_IN" *> pure VALUE_IN)
  <|> (keyword1 "VALUE_UNIQUE" *> pure VALUE_UNIQUE)

-- function_id .
pFunctionRef :: Parser FunctionRef
pFunctionRef = lexeme pFunctionId

-- simple_id .
pFunctionId :: Parser FunctionId
pFunctionId = lexeme pSimpleId

-- ' ( ' parameter { ' , ' parameter } ' ) ' .
pActualParameterList :: Parser ActualParameterList
pActualParameterList =
  ActualParameterList <$> (parens $ (lexeme pParameter) `sepBy1` commaSep)

-- expression .
pParameter :: Parser Parameter
pParameter = lexeme pExpression

-- parameter_ref | variable_ref .
--
-- It's all SimpleId in the end
pGeneralRef :: Parser GeneralRef
pGeneralRef = lexeme pSimpleId

-- entity_ref .
pPopulation :: Parser Population
pPopulation = lexeme pEntityRef

-- attribute_id .
pAttributeRef :: Parser AttributeRef
pAttributeRef = lexeme pAttributeId

-- simple_id .
pAttributeId :: Parser AttributeId
pAttributeId = lexeme pSimpleId

-- index .
pIndex1 :: Parser Index1
pIndex1 = lexeme pIndex

-- index .
pIndex2 :: Parser Index2
pIndex2 = lexeme pIndex

-- numeric_expression .
pIndex :: Parser Index
pIndex = lexeme pNumericExpression

-- simple_expression .
--
-- We consolidated 'expression' and 'simple_expression', so the burden of
-- distinguishing the two now lays on the later stages of analysis.
pNumericExpression :: Parser NumericExpression
pNumericExpression = lexeme pExpression
