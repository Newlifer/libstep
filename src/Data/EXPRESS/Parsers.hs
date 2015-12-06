{-# LANGUAGE OverloadedStrings #-}

module Data.EXPRESS.Parsers (
) where

import Prelude hiding (takeWhile)

import Control.Applicative (optional)
import Data.Attoparsec.ByteString
import Data.Either
import Data.Word

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

-- schema_decl { schema_decl } .
pExpress :: Parser Express
pExpress = do
  schemas <- many1 pSchema
  return $ Express schemas

-- SCHEMA schema_id [ schema_version_id ] ' ; ' schema_body END_SCHEMA ' ; ' .
pSchema :: Parser Schema
pSchema = do
  id <- pSchemaId
  versionId <- optional pSchemaVersionId
  body <- pSchemaBody
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
  interfaces' <- many' pInterfaceSpecification
  let interfaces = listToMaybe interfaces'
  constants <- optional pConstantDecl
  other <- many' (eitherP pDeclaration pRuleDecl)
  let declarations = listToMaybe $ lefts other
  let rules = listToMaybe $ rights other
  return $ SchemaBody interfaces constants declarations rules

-- letter { letter | digit | ' _ ' } .
pSimpleId :: Parser SimpleId
pSimpleId = do
  start <- takeWhile isLetter
  if BS.null start
    then return $ T.empty
    else do
      rest <- takeWhile (\x -> isLetter x || isDigit x || isUnderscore x)
      return $ TE.decodeUtf32BE (start `BS.append` rest)

  where
  isUnderscore :: Word8 -> Bool
  isUnderscore ch = ch == 0x5F

isLetter, isDigit :: Word8 -> Bool
isLetter ch = (ch >= 0x41 && ch <= 0x5A) -- capital letters
           || (ch >= 0x61 && ch <= 0x7A) -- small letters

isDigit ch = ch >= 0x30 && ch <= 0x39

{- \q { ( \q \q ) | not_quote | \s | \x9 | \xA | \xD } \q .

     or

   ' " ' encoded_character { encoded_character } ' " ' . -}
pStringLiteral :: Parser StringLiteral
pStringLiteral = choice [pSimpleStringLiteral, pEncodedStringLiteral]
  where
  -- \q { ( \q \q ) | not_quote | \s | \x9 | \xA | \xD } \q .
  pSimpleStringLiteral :: Parser T.Text
  pSimpleStringLiteral = do
    word8 apostrophe
    literal <- many' $ choice [pQQ, pNotQuote, pOther]
    word8 apostrophe
    return $ TE.decodeUtf32BE $ BS.concat literal

    where
    apostrophe :: Word8
    apostrophe = 0x27

    pQQ :: Parser BS.ByteString
    pQQ = do
      word8 apostrophe
      word8 apostrophe
      return $ BS.pack [apostrophe, apostrophe]

    pNotQuote :: Parser BS.ByteString
    pNotQuote = takeWhile okay
      where
      okay x = (isLetter x)
            || (isDigit x)
            -- ! " # $ % & ' ( ) * + , - . /
            || (x >= 0x21 && x <= 0x2F)
            -- : ; < = > ?
            || (x >= 0x3A && x <= 0x3F)
            -- @
            ||  x == 0x40
            -- [ \ ] ^
            || (x >= 0x5B && x <= 0x5E)
            -- `
            ||  x == 0x60
            -- { | } ~
            || (x >= 0x7B && x <= 0x7E)

    -- \s | \x9 | \xA | \xD
    pOther :: Parser BS.ByteString
    pOther = do
      ch <- satisfy okay
      return $ BS.singleton ch
      where
      okay x = x == 0x20 -- space
            || x == 0x09 -- tab
            || x == 0x0A -- LF
            || x == 0x0D -- CR

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
    pHexDigit = satisfy (\x -> (isDigit x) || (x >= 0x61 && x <= 0x66))

-- reference_clause | use_clause .
pInterfaceSpecification :: Parser InterfaceSpecification
pInterfaceSpecification = choice [pReferenceClause, pUseClause]

-- REFERENCE FROM schema_ref [ ' ( ' resource_or_rename { ' , ' resource_or_rename } ' ) ' ] ' ; ' .
pReferenceClause :: Parser InterfaceSpecification
pReferenceClause = do
  string "REFERENCE FROM"
  schemaRef <- pSchemaRef
  resources <- optional $ do
    string "("
    first <- pResourceOrRename
    rest <- many' $ string "," >> pResourceOrRename
    string ")"
    return $ first : rest
  return $ ReferenceClause schemaRef resources

-- USE FROM schema_ref [ ' ( ' named_type_or_rename { ' , ' named_type_or_rename } ' ) ' ] ' ; ' .
pUseClause :: Parser InterfaceSpecification
pUseClause = do
  string "USE FROM"
  schemaRef <- pSchemaRef
  namedTypesOrRenames <- optional $ do
    string "("
    first <- pNamedTypeOrRename
    rest <- many' $ string "," >> pNamedTypeOrRename
    string ")"
    return $ first : rest
  return $ UseClause schemaRef namedTypesOrRenames

-- schema_id .
pSchemaRef :: Parser SchemaRef
pSchemaRef = pSchemaId

-- resource_ref [ AS rename_id ] .
pResourceOrRename :: Parser ResourceOrRename
pResourceOrRename = do
  resourceRef <- pResourceRef
  renameId <- optional pRenameId
  return $ ResourceOrRename resourceRef renameId

-- constant_ref | entity_ref | function_ref | procedure_ref | type_ref .
--
-- Since all refs are mapped into ids, which in turn are equivalent to
-- simple_id, we just skip to the end.
pResourceRef :: Parser ResourceRef
pResourceRef = pSimpleId

-- constant_id .
pConstantRef :: Parser ConstantRef
pConstantRef = pConstantId

-- entity_id .
pEntityRef :: Parser EntityRef
pEntityRef = pEntityId

-- function_id .
pFunctionRef :: Parser FunctionRef
pFunctionRef = pFunctionId

-- procedure_id .
pProcedureRef :: Parser ProcedureRef
pProcedureRef = pProcedureId

-- type_id .
pTypeRef :: Parser TypeRef
pTypeRef = pTypeId

-- constant_id | entity_id | function_id | procedure_id | type_id .
--
-- In the end, all the IDs are SimpleId, so let's just skip the intermediate
-- step.
pRenameId :: Parser RenameId
pRenameId = pSimpleId

-- simple_id .
pConstantId :: Parser ConstantId
pConstantId = pSimpleId

-- simple_id .
pEntityId :: Parser EntityId
pEntityId = pSimpleId

-- simple_id .
pFunctionId :: Parser FunctionId
pFunctionId = pSimpleId

-- simple_id .
pProcedureId :: Parser ProcedureId
pProcedureId = pSimpleId

-- simple_id .
pTypeId :: Parser TypeId
pTypeId = pSimpleId

-- CONSTANT constant_body { constant_body } END_CONSTANT ' ; ' .
pConstantDecl :: Parser ConstantDecl
pConstantDecl = do
  string "CONSTANT"
  body <- many1 pConstantBody
  string "END_CONSTANT;"
  return $ ConstantDecl (listToMaybe body)

-- constant_id ' : ' instantiable_type ' := ' expression ' ; ' .
pConstantBody :: Parser ConstantBody
pConstantBody = undefined

-- binary_type | boolean_type | integer_type | logical_type | number_type | real_type | string_type .
pSimpleTypes :: Parser SimpleTypes
pSimpleTypes = undefined

-- simple_expression [ rel_op_extended simple_expression ] .
pExpression :: Parser Expression
pExpression = undefined

-- entity_decl | function_decl | procedure_decl | subtype_constraint_decl | type_decl .
pDeclaration :: Parser Declaration
pDeclaration = undefined

-- ENTITY entity_id subsuper ' ; ' .
pEntityHead :: Parser EntityHead
pEntityHead = undefined

-- { explicit_attr } [ derive_clause ] [ inverse_clause ] [ unique_clause ] [ where_clause ] .
pEntityBody :: Parser EntityBody
pEntityBody = undefined

-- [ supertype_constraint ] [ subtype_declaration ] .
pSubSuper :: Parser SubSuper
pSubSuper = undefined

-- abstract_entity_declaration | abstract_supertype_declaration | supertype_rule .
pSupertypeConstraint :: Parser SupertypeConstraint
pSupertypeConstraint = undefined

-- SUBTYPE OF ' ( ' entity_ref { ' , ' entity_ref } ' ) ' .
pSubtypeDeclaration :: Parser SubtypeDeclaration
pSubtypeDeclaration = undefined

-- OF ' ( ' supertype_expression ' ) ' .
pSubtypeConstraint :: Parser SubtypeConstraint
pSubtypeConstraint = undefined

-- supertype_factor { ANDOR supertype_factor } .
pSupertypeExpression :: Parser SupertypeExpression
pSupertypeExpression = undefined

-- supertype_term { AND supertype_term } .
pSupertypeFactor :: Parser SupertypeFactor
pSupertypeFactor = undefined

-- ONEOF ' ( ' supertype_expression { ' , ' supertype_expression } ' ) ' .
pOneOf :: Parser OneOf
pOneOf = undefined

-- WHERE domain_rule ' ; ' { domain_rule ' ; ' } .
pWhereClause :: Parser WhereClause
pWhereClause = undefined

-- [ rule_label_id ' : ' ] expression .
pDomainRule :: Parser DomainRule
pDomainRule = undefined

-- simple_id .
pRuleLabelId :: Parser RuleLabelId
pRuleLabelId = pSimpleId

-- UNIQUE unique_rule ' ; ' { unique_rule ' ; ' } .
pUniqueClause :: Parser UniqueClause
pUniqueClause = undefined

-- [ rule_label_id ' : ' ] referenced_attribute { ' , ' referenced_attribute } .
pUniqueRule :: Parser UniqueRule
pUniqueRule = undefined

-- SELF group_qualifier attribute_qualifier .
pQualifiedAttribute :: Parser QualifiedAttribute
pQualifiedAttribute = undefined

-- ' . ' attribute_ref .
pAttributeQualifier :: Parser AttributeQualifier
pAttributeQualifier = undefined

-- attribute_id .
pAttributeRef :: Parser AttributeRef
pAttributeRef = pAttributeId

-- simple_id .
pAttributeId :: Parser AttributeId
pAttributeId = pSimpleId

-- ' \ ' entity_ref .
pGroupQualifier :: Parser GroupQualifier
pGroupQualifier = undefined

-- INVERSE inverse_attr { inverse_attr } .
pInverseClause :: Parser InverseClause
pInverseClause = undefined

-- attribute_decl ' : ' [ ( SET | BAG ) [ bound_spec ] OF ] entity_ref FOR [ entity_ref ' . ' ] attribute_ref ' ; ' .
pInverseAttr :: Parser InverseAttr
pInverseAttr = undefined

-- ' [ ' bound_1 ' : ' bound_2 ' ] ' .
pBoundSpec :: Parser BoundSpec
pBoundSpec = undefined

-- numeric_expression .
pBound1 :: Parser Bound1
pBound1 = pNumericExpression

-- numeric_expression .
pBound2 :: Parser Bound2
pBound2 = pNumericExpression

-- simple_expression .
pNumericExpression :: Parser NumericExpression
pNumericExpression = pSimpleExpression

-- term { add_like_op term } .
pSimpleExpression :: Parser SimpleExpression
pSimpleExpression = undefined

-- factor { multiplication_like_op factor } .
pTerm :: Parser Term
pTerm = undefined

-- simple_factor [ ' ** ' simple_factor ] .
pFactor :: Parser Factor
pFactor = undefined

-- ' [ ' index_1 [ ' : ' index_2 ] ' ] ' .
pIndexQualifier :: Parser IndexQualifier
pIndexQualifier = undefined

-- index .
pIndex1 :: Parser Index1
pIndex1 = pIndex

-- index .
pIndex2 :: Parser Index2
pIndex2 = pIndex

-- numeric_expression .
pIndex :: Parser Index
pIndex = pNumericExpression

-- ( built_in_function | function_ref ) [ actual_parameter_list ] .
pFunctionCall :: Parser FunctionCall
pFunctionCall = undefined

-- parameter_ref | variable_ref .
--
-- It's all SimpleId in the end
pGeneralRef :: Parser GeneralRef
pGeneralRef = pSimpleId

-- entity_ref .
pPopulation :: Parser Population
pPopulation = pEntityRef

-- variable_id .
pVariableRef :: Parser VariableRef
pVariableRef = pVariableId

-- simple_id .
pVariableId :: Parser VariableId
pVariableId = pSimpleId

-- parameter_id .
pParameterRef :: Parser ParameterRef
pParameterRef = pParameterId

-- simple_id .
pParameterId :: Parser ParameterId
pParameterId = pSimpleId

-- ' ( ' parameter { ' , ' parameter } ' ) ' .
pActualParameterList :: Parser ActualParameterList
pActualParameterList = undefined

-- expression .
pParameter :: Parser Parameter
pParameter = pExpression

-- ABS | ACOS | ASIN | ATAN | BLENGTH | COS | EXISTS | EXP | FORMAT | HIBOUND | HIINDEX | LENGTH | LOBOUND | LOINDEX | LOG | LOG2 | LOG10 | NVL | ODD | ROLESOF | SIN | SIZEOF | SQRT | TAN | TYPEOF | USEDIN | VALUE | VALUE_IN | VALUE_UNIQUE .
pBuiltInFunction :: Parser BuiltInFunction
pBuiltInFunction = undefined

-- CONST_E | PI | SELF | ' ? ' .
pBuiltInConstant :: Parser BuiltInConstant
pBuiltInConstant = undefined

-- nteger_literal | ( digits ' . ' [ digits ] [ ' e ' [ sign ] digits ] ) .
pRealLiteral :: Parser RealLiteral
pRealLiteral = undefined

-- FALSE | TRUE | UNKNOWN .
pLogicalLiteral :: Parser LogicalLiteral
pLogicalLiteral = undefined

-- ' % ' bit { bit } .
pBinaryLiteral :: Parser BinaryLiteral
pBinaryLiteral = undefined

-- ' 0 ' | ' 1 ' .
pBit :: Parser Bit
pBit = undefined

-- ' + ' | ' - ' | NOT .
pUnaryOp :: Parser UnaryOp
pUnaryOp = undefined

-- QUERY ' ( ' variable_id ' <* ' aggregate_source ' | ' logical_expression ' ) ' .
pQueryExpression :: Parser QueryExpression
pQueryExpression = undefined

-- expression .
pLogicalExpression :: Parser LogicalExpression
pLogicalExpression = pExpression

-- simple_expression .
pAggregateSource :: Parser AggregateSource
pAggregateSource = pSimpleExpression

-- ' { ' interval_low interval_op interval_item interval_op interval_high ' } ' .
pInterval :: Parser Interval
pInterval = undefined

-- simple_expression .
pIntervalLow :: Parser IntervalLow
pIntervalLow = pSimpleExpression

-- simple_expression .
pIntervalHigh :: Parser IntervalHigh
pIntervalHigh = pSimpleExpression

-- simple_expression .
pIntervalItem :: Parser IntervalItem
pIntervalItem = pSimpleExpression

-- ' < ' | ' <= ' .
pIntervalOp :: Parser IntervalOp
pIntervalOp = undefined

-- [ type_ref ' . ' ] enumeration_ref .
pEnumerationReference :: Parser EnumerationReference
pEnumerationReference = undefined

-- enumeration_id .
pEnumerationRef :: Parser EnumerationRef
pEnumerationRef = pEnumerationId

-- simple_id .
pEnumerationId :: Parser EnumerationId
pEnumerationId = pSimpleId

-- entity_ref ' ( ' [ expression { ' , ' expression } ] ' ) ' .
pEntityConstructor :: Parser EntityConstructor
pEntityConstructor = undefined

-- ' [ ' [ element { ' , ' element } ] ' ] ' .
pAggregateInitializer :: Parser AggregateInitializer
pAggregateInitializer = undefined

-- expression [ ' : ' repetition ] .
pElement :: Parser Element
pElement = undefined

-- numeric_expression .
pRepetition :: Parser Repetition
pRepetition = pNumericExpression

-- ' * ' | ' / ' | DIV | MOD | AND | ' || ' .
-- `Or` is `||`
pMultiplicationLikeOp :: Parser MultiplicationLikeOp
pMultiplicationLikeOp = undefined

-- ' + ' | ' - ' | OR | XOR .
pAddLikeOp :: Parser AddLikeOp
pAddLikeOp = undefined

-- qualified_attribute [ RENAMED attribute_id ] .
pRedeclaredAttribute :: Parser RedeclaredAttribute
pRedeclaredAttribute = undefined

-- DERIVE derived_attr { derived_attr } .
pDeriveClause :: Parser DeriveClause
pDeriveClause = undefined

-- attribute_decl ' : ' parameter_type ' := ' expression ' ; ' .
pDerivedAttr :: Parser DerivedAttr
pDerivedAttr = undefined

-- entity_ref | type_ref .
--
-- In the end, it's SimpleId.
pNamedTypes :: Parser NamedTypes
pNamedTypes = pSimpleId

-- GENERIC [ ' : ' type_label ] .
pGenericType :: Parser GenericType
pGenericType = undefined

-- type_label_id | type_label_ref .
--
-- In the end, it all comes down to SimpleId.
pTypeLabel :: Parser TypeLabel
pTypeLabel = pSimpleId

-- type_label_id .
pTypeLabelRef :: Parser TypeLabelRef
pTypeLabelRef = pTypeLabelId

-- simple_id .
pTypeLabelId :: Parser TypeLabelId
pTypeLabelId = pSimpleId

-- GENERIC_ENTITY [ ' : ' type_label ] .
pGenericEntityType :: Parser GenericEntityType
pGenericEntityType = undefined

-- ARRAY [ bound_spec ] OF [ OPTIONAL ] [ UNIQUE ] parameter_type .
pGeneralArrayType :: Parser GeneralArrayType
pGeneralArrayType = undefined

-- BAG [ bound_spec ] OF parameter_type .
pGeneralBagType :: Parser GeneralBagType
pGeneralBagType = undefined

-- LIST [ bound_spec ] OF [ UNIQUE ] parameter_type .
pGeneralListType :: Parser GeneralListType
pGeneralListType = undefined

-- SET [ bound_spec ] OF parameter_type .
pGeneralSetType :: Parser GeneralSetType
pGeneralSetType = undefined

-- AGGREGATE [ ' : ' type_label ] OF parameter_type .
pAggregateType :: Parser AggregateType
pAggregateType = undefined

-- attribute_decl { ' , ' attribute_decl } ' : ' [ OPTIONAL ] parameter_type ' ; ' .
pExplicitAttr :: Parser ExplicitAttr
pExplicitAttr = undefined

-- enumeration_type | select_type .
pConstructedTypes :: Parser ConstructedTypes
pConstructedTypes = undefined

-- BASED_ON type_ref [ WITH select_list ] .
pSelectExtension :: Parser SelectExtension
pSelectExtension = undefined

-- ' ( ' named_types { ' , ' named_types } ' ) ' .
pSelectList :: Parser SelectList
pSelectList = undefined

-- BASED_ON type_ref [ WITH enumeration_items ] .
pEnumerationExtension :: Parser EnumerationExtension
pEnumerationExtension = undefined

-- ' ( ' enumeration_id { ' , ' enumeration_id } ' ) ' .
pEnumerationItems :: Parser EnumerationItems
pEnumerationItems = undefined

-- SUBTYPE_CONSTRAINT subtype_constraint_id FOR entity_ref ' ; ' .
pSubtypeConstraintHead :: Parser SubtypeConstraintHead
pSubtypeConstraintHead = undefined

-- [ abstract_supertype ] [ total_over ] [ supertype_expression ' ; ' ] .
pSubtypeConstraintBody :: Parser SubtypeConstraintBody
pSubtypeConstraintBody = undefined

-- TOTAL_OVER ' ( ' entity_ref { ' , ' entity_ref } ' ) ' ' ; ' .
pTotalOver :: Parser TotalOver
pTotalOver = undefined

-- ABSTRACT SUPERTYPE ' ; ' .
pAbstractSupertype :: Parser AbstractSupertype
pAbstractSupertype = undefined

-- simple_id .
pSubtypeConstraintId :: Parser SubtypeConstraintId
pSubtypeConstraintId = pSimpleId

-- alias_stmt | assignment_stmt | case_stmt | compound_stmt | escape_stmt | if_stmt | null_stmt | procedure_call_stmt | repeat_stmt | return_stmt | skip_stmt .
pStmt :: Parser Stmt
pStmt = undefined

-- [ increment_control ] [ while_control ] [ until_control ] .
pRepeatControl :: Parser RepeatControl
pRepeatControl = undefined

-- UNTIL logical_expression .
pUntilControl :: Parser UntilControl
pUntilControl = undefined

-- WHILE logical_expression .
pWhileControl :: Parser WhileControl
pWhileControl = undefined

-- variable_id ' := ' bound_1 TO bound_2 [ BY increment ] .
pIncrementControl :: Parser IncrementControl
pIncrementControl = undefined

-- numeric_expression .
pIncrement :: Parser Increment
pIncrement = pNumericExpression

-- INSERT | REMOVE .
pBuiltInProcedure :: Parser BuiltInProcedure
pBuiltInProcedure = undefined

-- numeric_expression .
pWidth :: Parser Width
pWidth = pNumericExpression

-- case_label { ' , ' case_label } ' : ' stmt .
pCaseAction :: Parser CaseAction
pCaseAction = undefined

-- expression .
pCaseLabel :: Parser CaseLabel
pCaseLabel = pExpression

-- expression .
pSelector :: Parser Selector
pSelector = pExpression

-- { declaration } [ constant_decl ] [ local_decl ] .
pAlgorithmHead :: Parser AlgorithmHead
pAlgorithmHead = undefined

-- LOCAL local_variable { local_variable } END_LOCAL ' ; ' .
pLocalDecl :: Parser LocalDecl
pLocalDecl = undefined

-- variable_id { ' , ' variable_id } ' : ' parameter_type [ ' := ' expression ] ' ; ' .
pLocalVariable :: Parser LocalVariable
pLocalVariable = undefined

-- PROCEDURE procedure_id [ ' ( ' [ VAR ] formal_parameter { ' ; ' [ VAR ] formal_parameter } ' ) ' ] ' ; ' .
pProcedureHead :: Parser ProcedureHead
pProcedureHead = undefined

-- parameter_id { ' , ' parameter_id } ' : ' parameter_type .
pFormalParameter :: Parser FormalParameter
pFormalParameter = undefined

-- FUNCTION function_id [ ' ( ' formal_parameter { ' ; ' formal_parameter } ' ) ' ] ' : ' parameter_type ' ; ' .
pFunctionHead :: Parser FunctionHead
pFunctionHead = undefined

-- rel_op | IN | LIKE .
pRelOpExtended :: Parser RelOpExtended
pRelOpExtended = undefined

-- ' ( ' width ' ) ' [ FIXED ] .
pWidthSpec :: Parser WidthSpec
pWidthSpec = undefined

-- numeric_expression .
pPrecisionSpec :: Parser PrecisionSpec
pPrecisionSpec = pNumericExpression

-- SET [ bound_spec ] OF instantiable_type .
pSetType :: Parser SetType
pSetType = undefined

-- LIST [ bound_spec ] OF [ UNIQUE ] instantiable_type .
pListType :: Parser ListType
pListType = undefined

-- BAG [ bound_spec ] OF instantiable_type .
pBagType :: Parser BagType
pBagType = undefined

-- ARRAY bound_spec OF [ OPTIONAL ] [ UNIQUE ] instantiable_type .
pArrayType :: Parser ArrayType
pArrayType = undefined

-- named_types [ AS ( entity_id | type_id ) ] .
pNamedTypeOrRename :: Parser NamedTypeOrRename
pNamedTypeOrRename = undefined

-- rule_head algorithm_head { stmt } where_clause END_RULE ' ; ' .
pRuleDecl :: Parser RuleDecl
pRuleDecl = undefined

-- RULE rule_id FOR ' ( ' entity_ref { ' , ' entity_ref } ' ) ' ' ; ' .
pRuleHead :: Parser RuleHead
pRuleHead = undefined

-- simple_id .
pRuleId :: Parser RuleId
pRuleId = pSimpleId
