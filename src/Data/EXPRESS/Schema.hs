{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.EXPRESS.Schema where

{- Lists are implied to be one or more elements long. If list can be empty, we
 - use Maybe [...] instead.
 -}

import Data.Scientific (Scientific())
import Data.Text (Text)

-- schema_decl { schema_decl } .
data Express = Express [Schema]
  deriving (Eq, Show)

-- SCHEMA schema_id [ schema_version_id ] ' ; ' schema_body END_SCHEMA ' ; ' .
data Schema = Schema {
  sId      :: SchemaId
, sVersion :: Maybe SchemaVersionId
, sBody    :: SchemaBody
}
  deriving (Eq, Show)

-- simple_id .
type SchemaId = SimpleId

-- string_literal .
type SchemaVersionId = StringLiteral

-- { interface_specification } [ constant_decl ] { declaration | rule_decl } .
data SchemaBody = SchemaBody {
  sInterfaces    :: Maybe [InterfaceSpecification]
, sConstants     :: Maybe ConstantDecl
-- , declaractions :: Maybe [Declaration]
-- , rules         :: Maybe [RuleDecl]
}
  deriving (Eq, Show)

-- letter { letter | digit | ' _ ' } .
type SimpleId = Text

{- \q { ( \q \q ) | not_quote | \s | \x9 | \xA | \xD } \q .

--      or

--    ' " ' encoded_character { encoded_character } ' " ' . -}
type StringLiteral = Text

-- reference_clause | use_clause .
data InterfaceSpecification =
    -- REFERENCE FROM schema_ref [ ' ( ' resource_or_rename { ' , ' resource_or_rename } ' ) ' ] ' ; ' .
    ReferenceClause {
      rcSchemaRef :: SchemaRef
    , rcResources :: Maybe [ResourceOrRename]
    }
    -- USE FROM schema_ref [ ' ( ' named_type_or_rename { ' , ' named_type_or_rename } ' ) ' ] ' ; ' .
  | UseClause {
      ucSchemaRef           :: SchemaRef
    , ucNamedTypesOrRenames :: Maybe [NamedTypeOrRename]
    }
  deriving (Eq, Show)

-- schema_id .
type SchemaRef = SchemaId

-- resource_ref [ AS rename_id ] .
data ResourceOrRename = ResourceOrRename {
  rorResourceRef :: ResourceRef
, rorRenameId    :: Maybe RenameId
}
  deriving (Eq, Show)

-- constant_ref | entity_ref | function_ref | procedure_ref | type_ref .
--
-- Since all refs are mapped into ids, which in turn are equivalent to
-- simple_id, we just skip to the end.
type ResourceRef = SimpleId

-- constant_id .
type ConstantRef = ConstantId

-- function_id .
type FunctionRef = FunctionId

-- procedure_id .
-- type ProcedureRef = ProcedureId

-- constant_id | entity_id | function_id | procedure_id | type_id .
--
-- In the end, all the IDs are SimpleId, so let's just skip the intermediate
-- step.
type RenameId = SimpleId

-- simple_id .
type ConstantId = SimpleId

-- simple_id .
type EntityId = SimpleId

-- simple_id .
type FunctionId = SimpleId

-- simple_id .
-- type ProcedureId = SimpleId

-- simple_id .
type TypeId = SimpleId

-- CONSTANT constant_body { constant_body } END_CONSTANT ' ; ' .
data ConstantDecl = ConstantDecl {
  cdBody :: [ConstantBody]
}
  deriving (Eq, Show)

-- constant_id ' : ' instantiable_type ' := ' expression ' ; ' .
data ConstantBody = ConstantBody {
  cbIid        :: ConstantId
, cbType       :: EXPRESSType
, cbExpression :: Expression
}
  deriving (Eq, Show)

data EXPRESSType =
    -- BINARY [ width_spec ] .
    BinaryType {
      btWidthSpec :: Maybe WidthSpec
    }
    -- BOOLEAN .
  | BooleanType
    -- INTEGER .
  | IntegerType
    -- LOGICAL .
  | LogicalType
    -- NUMBER .
  | NumberType
    -- REAL [ ' ( ' precision_spec ' ) ' ] .
  | RealType {
      rtPrecisionSpec :: Maybe PrecisionSpec
    }
    -- STRING [ width_spec ] .
  | StringType {
      stWidthSpec :: Maybe WidthSpec
    }
    -- SET [ bound_spec ] OF instantiable_type .
  | SetType {
      stBounds :: Maybe BoundSpec
    , stType   :: EXPRESSType
    }
    -- LIST [ bound_spec ] OF [ UNIQUE ] instantiable_type .
  | ListType {
      ltBoundSpec :: Maybe BoundSpec
    , ltUnique    :: Bool
    , ltType      :: EXPRESSType
    }
    -- BAG [ bound_spec ] OF instantiable_type .
  | BagType {
      btBoundSpec :: Maybe BoundSpec
    , btType      :: EXPRESSType
    }
    -- ARRAY bound_spec OF [ OPTIONAL ] [ UNIQUE ] instantiable_type .
  | ArrayType {
      atBounds   :: BoundSpec
    , atOptional :: Bool
    , atUnique   :: Bool
    , atType     :: EXPRESSType
    }
    -- entity_id .
  | EntityRef {
      erId :: EntityId
    }
    -- type_id .
  | TypeRef {
      trId :: TypeId
    }
  deriving (Eq, Show)

data Expression = ELiteral         Literal
                  -- ' [ ' [ element { ' , ' element } ] ' ] ' .
                | AggregateInitializer {
                    aiElements :: Maybe [Element]
                  }
                  -- entity_ref ' ( ' [ expression { ' , ' expression } ] ' ) ' .
                | EntityConstructor {
                    ecEntityRef   :: EXPRESSType
                  , ecExpressions :: Maybe [Expression]
                  }
                  -- [ type_ref ' . ' ] enumeration_ref .
                | EnumerationReference {
                    erTypeRef        :: Maybe EXPRESSType
                  , erEnumerationRef :: EnumerationRef
                  }
                  -- ' { ' interval_low interval_op interval_item interval_op interval_high ' } ' .
                | Interval {
                    intervalLow  :: IntervalLow
                  , intervalOp   :: IntervalOp
                  , intervalItem :: IntervalItem
                  , intervalOp2  :: IntervalOp
                  , intervalHigh :: IntervalHigh
                  }
                  -- QUERY ' ( ' variable_id ' <* ' aggregate_source ' | ' logical_expression ' ) ' .
                | QueryExpression {
                    qeVariableId        :: VariableId
                  , qeAggregateSource   :: AggregateSource
                  , qeLogicalExpression :: LogicalExpression
                  }
                | Negate           Expression
                | Not              Expression
                | Add              Expression  Expression
                | Subtract         Expression  Expression
                | Multiply         Expression  Expression
                | Divide           Expression  Expression
                | Pow              Expression  Expression
                | Div              Expression  Expression
                | Mod              Expression  Expression
                | And              Expression  Expression
                | Or               Expression  Expression
                | Compose          Expression  Expression
                | Xor              Expression  Expression
                | Equals           Expression  Expression
                | NotEquals        Expression  Expression
                | LessOrEquals     Expression  Expression
                | GreaterOrEquals  Expression  Expression
                | Less             Expression  Expression
                | Greater          Expression  Expression
                | WeirdEquals      Expression  Expression
                | WeirdNotEquals   Expression  Expression
                | In               Expression  Expression
                | Like             Expression  Expression
  deriving (Eq, Show)

-- entity_decl | function_decl | procedure_decl | subtype_constraint_decl | type_decl .
-- data Declaration =
--     -- entity_head entity_body END_ENTITY ' ; ' .
--     EntityDecl {
--       ehead :: EntityHead
--     , ebody :: EntityBody
--     }
--     -- function_head algorithm_head stmt { stmt } END_FUNCTION ' ; ' .
--   | FunctionDecl {
--       fDFunctionHead  :: FunctionHead
--     , fDAlgorithmHead :: AlgorithmHead
--     , fDStatements    :: [Stmt]
--     }

--     -- procedure_head algorithm_head { stmt } END_PROCEDURE ' ; ' .
--   | ProcedureDecl {
--       procedureHead :: ProcedureHead
--     , algorithmHead :: AlgorithmHead
--     , stmts         :: Maybe [Stmt]
--     }

--     -- subtype_constraint_head subtype_constraint_body END_SUBTYPE_CONSTRAINT ' ; ' .
--   | SubtypeConstraintDecl {
--       subtypeConstraintHead :: SubtypeConstraintHead
--     , subtypeConstraintBody :: SubtypeConstraintBody
--     }

--     -- TYPE type_id ' = ' underlying_type ' ; ' [ where_clause ] END_TYPE ' ; ' .
--   | TypeDecl {
--       tDTypeId       :: TypeId
--     , underlyingType :: UnderlyingType
--     , whereClause    :: Maybe WhereClause
--     }
--   deriving (Eq, Show)

-- ENTITY entity_id subsuper ' ; ' .
-- data EntityHead = EntityHead {
--   eid      :: EntityId
-- , subsuper :: SubSuper
-- }
--   deriving (Eq, Show)

-- { explicit_attr } [ derive_clause ] [ inverse_clause ] [ unique_clause ] [ where_clause ] .
-- data EntityBody = EntityBody {
--   eBAttrs       :: Maybe [ExplicitAttr]
-- , deriveClause  :: Maybe DeriveClause
-- , inverseClause :: Maybe InverseClause
-- , uniqueClause  :: Maybe UniqueClause
-- , eBWhereClause :: Maybe WhereClause
-- }
--   deriving (Eq, Show)

-- [ supertype_constraint ] [ subtype_declaration ] .
-- data SubSuper = SubSuper {
--   sup :: Maybe SupertypeConstraint
-- , sub :: Maybe SubtypeDeclaration
-- }
--   deriving (Eq, Show)

-- abstract_entity_declaration | abstract_supertype_declaration | supertype_rule .
-- data SupertypeConstraint =
--     -- ABSTRACT .
--     AbstractEntityDeclaration

--     -- ABSTRACT SUPERTYPE [ subtype_constraint ] .
--   | AbstractSupertypeDeclaration {
--       subtypeConstraint :: Maybe SubtypeConstraint
--     }

--     -- SUPERTYPE subtype_constraint .
--   | SupertypeRule {
--       srSubtypeConstraint :: SubtypeConstraint
--     }
--   deriving (Eq, Show)

-- SUBTYPE OF ' ( ' entity_ref { ' , ' entity_ref } ' ) ' .
-- data SubtypeDeclaration = SubtypeDeclaration {
--   subtypeOf :: [EntityRef]
-- }
--   deriving (Eq, Show)

-- OF ' ( ' supertype_expression ' ) ' .
-- data SubtypeConstraint = SubtypeConstraint {
--   scSupertypeExpression :: SupertypeExpression
-- }
--   deriving (Eq, Show)

-- supertype_factor { ANDOR supertype_factor } .
-- data SupertypeExpression where
--   SEFactor :: SupertypeFactor -> SupertypeExpression
--   SEAndOr :: SupertypeFactor -> SupertypeFactor -> SupertypeExpression
--   deriving (Eq, Show)

-- supertype_term { AND supertype_term } .
-- data SupertypeFactor where
--   STTerm :: SupertypeTerm a => a -> SupertypeFactor
--   And :: (SupertypeTerm a, SupertypeTerm b) => a -> b -> SupertypeFactor
--   deriving (Eq, Show)

-- entity_ref | one_of | ' ( ' supertype_expression ' ) ' .
-- class SupertypeTerm a
-- instance SupertypeTerm EntityRef
-- instance SupertypeTerm OneOf
-- instance SupertypeTerm SupertypeExpression

-- ONEOF ' ( ' supertype_expression { ' , ' supertype_expression } ' ) ' .
-- data OneOf = OneOf {
--   oOExpressions :: [SupertypeExpression]
-- }
--   deriving (Eq, Show)

-- WHERE domain_rule ' ; ' { domain_rule ' ; ' } .
-- data WhereClause = WhereClause {
--   domainRules :: [DomainRule]
-- }
--   deriving (Eq, Show)

-- [ rule_label_id ' : ' ] expression .
-- data DomainRule = DomainRule {
--   dRRuleLabelId :: Maybe RuleLabelId
-- , expr          :: Expression
-- }
--   deriving (Eq, Show)

-- simple_id .
-- type RuleLabelId = SimpleId

-- UNIQUE unique_rule ' ; ' { unique_rule ' ; ' } .
-- data UniqueClause = UniqueClause {
--   uniqueRule :: [UniqueRule]
-- }
--   deriving (Eq, Show)

-- [ rule_label_id ' : ' ] referenced_attribute { ' , ' referenced_attribute } .
-- data UniqueRule where
--   UniqueRule :: ReferencedAttribute a => Maybe RuleLabelId -> a -> UniqueRule
--   deriving (Eq, Show)

-- attribute_ref | qualified_attribute .
-- class ReferencedAttribute a
-- instance ReferencedAttribute AttributeRef
-- instance ReferencedAttribute QualifiedAttribute

-- SELF group_qualifier attribute_qualifier .
-- data QualifiedAttribute = QualifiedAttribute {
--   groupQualifier     :: GroupQualifier
-- , attributeQualifier :: AttributeQualifier
-- }
--   deriving (Eq, Show)

-- INVERSE inverse_attr { inverse_attr } .
-- data InverseClause = InverseClause {
--   inverseAttrs :: [InverseAttr]
-- }
--   deriving (Eq, Show)

-- attribute_decl ' : ' [ ( SET | BAG ) [ bound_spec ] OF ] entity_ref FOR [ entity_ref ' . ' ] attribute_ref ' ; ' .
-- data InverseAttr = InverseAttr {
--   iAAttributeDecl :: AttributeDecl
-- , setOrBagBounds  :: Maybe SetOrBagBounds
-- , iAEntityRef     :: EntityRef
-- , forEntityRef    :: Maybe EntityRef
-- , attributeRef    :: AttributeRef
-- }
--   deriving (Eq, Show)

-- data SetOrBagBounds = SetOrBagBounds {
--   setOrBag   :: SetOrBag
-- , sOBBBounds :: Maybe BoundSpec
-- }
--   deriving (Eq, Show)

-- data SetOrBag = SetTag | BagTag
--   deriving (Eq, Show)

-- ' [ ' bound_1 ' : ' bound_2 ' ] ' .
data BoundSpec = BoundSpec {
  bsBound1 :: Bound1
, bsBound2 :: Bound2
}
  deriving (Eq, Show)

-- numeric_expression .
type Bound1 = NumericExpression

-- numeric_expression .
type Bound2 = NumericExpression

-- simple_expression .
type NumericExpression = Expression

-- attribute_id .
type AttributeRef = AttributeId

-- simple_id .
type AttributeId = SimpleId

-- index .
type Index1 = Index

-- index .
type Index2 = Index

-- numeric_expression .
type Index = NumericExpression

-- ( built_in_function | function_ref ) [ actual_parameter_list ] .
data FunctionCall = FunctionCall {
  fcBegin  :: Either BuiltInFunction FunctionRef
, fcParams :: Maybe ActualParameterList
}
  deriving (Eq, Show)

-- parameter_ref | variable_ref .
--
-- It's all SimpleId in the end
type GeneralRef = SimpleId

-- entity_ref .
type Population = EXPRESSType

-- variable_id .
type VariableRef = VariableId

-- simple_id .
type VariableId = SimpleId

-- parameter_id .
-- type ParameterRef = ParameterId

-- simple_id .
-- type ParameterId = SimpleId

-- ' ( ' parameter { ' , ' parameter } ' ) ' .
data ActualParameterList = ActualParameterList {
  aplParameters :: [Parameter]
}
  deriving (Eq, Show)

-- expression .
type Parameter = Expression

-- ABS | ACOS | ASIN | ATAN | BLENGTH | COS | EXISTS | EXP | FORMAT | HIBOUND | HIINDEX | LENGTH | LOBOUND | LOINDEX | LOG | LOG2 | LOG10 | NVL | ODD | ROLESOF | SIN | SIZEOF | SQRT | TAN | TYPEOF | USEDIN | VALUE | VALUE_IN | VALUE_UNIQUE .
data BuiltInFunction =
    ABS
  | ACOS
  | ASIN
  | ATAN
  | BLENGTH
  | COS
  | EXISTS
  | EXP
  | FORMAT
  | HIBOUND
  | HIINDEX
  | LENGTH
  | LOBOUND
  | LOINDEX
  | LOG
  | LOG2
  | LOG10
  | NVL
  | ODD
  | ROLESOF
  | SIN
  | SIZEOF
  | SQRT
  | TAN
  | TYPEOF
  | USEDIN
  | VALUE
  | VALUE_IN
  | VALUE_UNIQUE
  deriving (Eq, Show)

-- CONST_E | PI | SELF | ' ? ' .
data BuiltInConstant = CONST_E | PI | SELF | QuestionMark
  deriving (Eq, Show)

-- binary_literal | logical_literal | integer_literal | real_literal | string_literal .
data Literal =
    -- ' % ' bit { bit } .
    BinaryLiteral {
      blBits :: [Bit]
    }
    -- FALSE | TRUE | UNKNOWN .
  | LLogicalLiteral {
      lLogicalLiteral :: LogicalLiteral
    }
    -- integer_literal
  | IntegerLiteral {
      lIntegerLiteral :: Integer
    }
    -- digits ' . ' [ digits ] [ ' e ' [ sign ] digits ]
  | RealLiteral {
      lRealLiteral :: Scientific
    }
  | LStringLiteral {
      lStringLiteral :: StringLiteral
    }
  deriving (Eq, Show)

-- FALSE | TRUE | UNKNOWN .
data LogicalLiteral = FALSE | TRUE | UNKNOWN
  deriving (Eq, Show)

-- ' 0 ' | ' 1 ' .
data Bit = Zero | One
  deriving (Eq, Show)

-- ' + ' | ' - ' | NOT .
data UnaryOp = UPlus | UMinus | UNOT
  deriving (Eq, Show)

-- expression .
type LogicalExpression = Expression

-- simple_expression .
type AggregateSource = Expression

-- simple_expression .
type IntervalLow = Expression

-- simple_expression .
type IntervalHigh = Expression

-- simple_expression .
type IntervalItem = Expression

-- ' < ' | ' <= ' .
data IntervalOp =
    IntervalLess
  | IntervalLessEqual
  deriving (Eq, Show)

-- enumeration_id .
type EnumerationRef = EnumerationId

-- simple_id .
type EnumerationId = SimpleId

-- expression [ ' : ' repetition ] .
data Element = Element {
  eExpression :: Expression
, rRepetition :: Maybe Repetition
}
  deriving (Eq, Show)

-- numeric_expression .
type Repetition = NumericExpression

-- attribute_id | redeclared_attribute .
-- class CAttributeDecl a
-- instance CAttributeDecl AttributeId
-- instance CAttributeDecl RedeclaredAttribute

-- data AttributeDecl = forall a. CAttributeDecl a => AttributeDecl a
-- instance Eq AttributeDecl where
--   (AttributeDecl a) == (AttributeDecl b) = a == b

-- qualified_attribute [ RENAMED attribute_id ] .
-- data RedeclaredAttribute = RedeclaredAttribute {
--   qualifiedAttribute :: QualifiedAttribute
-- , attributeId        :: Maybe AttributeId
-- }
--   deriving (Eq, Show)

-- DERIVE derived_attr { derived_attr } .
-- data DeriveClause = DeriveClause {
--   derivedAttrs :: [DerivedAttr]
-- }
--   deriving (Eq, Show)

-- attribute_decl ' : ' parameter_type ' := ' expression ' ; ' .
-- data DerivedAttr = DerivedAttr {
--   attributeDecl   :: AttributeDecl
-- , dAParameterType :: ParameterType
-- , expression      :: Expression
-- }
--   deriving (Eq, Show)

-- generalized_types | named_types | simple_types .
-- class CParameterType a
-- instance CParameterType NamedTypes
-- instance GeneralizedTypes a => CParameterType a
-- instance CParameterType SimpleTypes

-- data ParameterType = forall a. CParameterType a => ParameterType a
-- instance Eq ParameterType where
--   (ParameterType a) == (ParameterType b) = a == b

-- entity_ref | type_ref .
--
-- In the end, it's SimpleId.
type NamedTypes = SimpleId

-- aggregate_type | general_aggregation_types | generic_entity_type | generic_type .
-- class GeneralizedTypes a
-- instance GeneralizedTypes AggregateType
-- instance GeneralAggregationTypes a => GeneralizedTypes a
-- instance GeneralizedTypes GenericEntityType
-- instance GeneralizedTypes GenericType

-- GENERIC [ ' : ' type_label ] .
-- data GenericType = GenericType {
--   gTTypeLabel :: Maybe TypeLabel
-- }
--   deriving (Eq, Show)

-- type_label_id | type_label_ref .
--
-- In the end, it all comes down to SimpleId.
-- type TypeLabel = SimpleId

-- type_label_id .
-- type TypeLabelRef = TypeLabelId

-- simple_id .
-- type TypeLabelId = SimpleId

-- GENERIC_ENTITY [ ' : ' type_label ] .
-- data GenericEntityType = GenericEntityType {
--   gETTypeLabel :: Maybe TypeLabel
-- }
--   deriving (Eq, Show)

-- general_array_type | general_bag_type | general_list_type | general_set_type .
-- class GeneralAggregationTypes a
-- instance GeneralAggregationTypes GeneralArrayType
-- instance GeneralAggregationTypes GeneralBagType
-- instance GeneralAggregationTypes GeneralListType
-- instance GeneralAggregationTypes GeneralSetType

-- ARRAY [ bound_spec ] OF [ OPTIONAL ] [ UNIQUE ] parameter_type .
-- data GeneralArrayType = GeneralArrayType {
--   gATBounds        :: Maybe BoundSpec
-- , gOptional        :: Bool
-- , gATUnique        :: Bool
-- , gATParameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- BAG [ bound_spec ] OF parameter_type .
-- data GeneralBagType = GeneralBagType {
--   gBTBounds        :: Maybe BoundSpec
-- , gBTParameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- LIST [ bound_spec ] OF [ UNIQUE ] parameter_type .
-- data GeneralListType = GeneralListType {
--   gLTBounds        :: Maybe BoundSpec
-- , unique           :: Bool
-- , gLTParameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- SET [ bound_spec ] OF parameter_type .
-- data GeneralSetType = GeneralSetType {
--   bounds        :: Maybe BoundSpec
-- , parameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- AGGREGATE [ ' : ' type_label ] OF parameter_type .
-- data AggregateType = AggregateType {
--   typeLabel       :: Maybe TypeLabel
-- , aTParameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- attribute_decl { ' , ' attribute_decl } ' : ' [ OPTIONAL ] parameter_type ' ; ' .
-- data ExplicitAttr = ExplicitAttr {
--   attributeDecls  :: [AttributeDecl]
-- , eAOptional      :: Bool
-- , eAParameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- concrete_types | constructed_types .
-- class CUnderlyingType a
-- instance ConcreteTypes a => CUnderlyingType a
-- instance CUnderlyingType ConstructedTypes

-- data UnderlyingType = forall a. CUnderlyingType a => UnderlyingType a

-- enumeration_type | select_type .
-- data ConstructedTypes =
--   -- [ EXTENSIBLE [ GENERIC_ENTITY ] ] SELECT [ select_list | select_extension ] .
--     SelectType {
--       extensibleGenericEntity :: Maybe (Extensible (Maybe GenericEntity))
--     , sTBody                  :: Maybe (Either SelectList SelectExtension)
--     }
--   -- [ EXTENSIBLE ] ENUMERATION [ ( OF enumeration_items ) | enumeration_extension ] .
--   | EnumerationType {
--       extensible :: Bool
--     , body       :: Maybe (Either EnumerationItems EnumerationExtension)
--     }
--   deriving (Eq, Show)

-- data Extensible a = Extensible a
--   deriving (Eq, Show)

-- data GenericEntity = GenericEntity
--   deriving (Eq, Show)

-- BASED_ON type_ref [ WITH select_list ] .
-- data SelectExtension = SelectExtension {
--   sETypeRef    :: TypeRef
-- , sESelectList :: Maybe SelectList
-- }
--   deriving (Eq, Show)

-- ' ( ' named_types { ' , ' named_types } ' ) ' .
-- data SelectList = SelectList {
--   sLNamedTypes :: [NamedTypes]
-- }
--   deriving (Eq, Show)


-- BASED_ON type_ref [ WITH enumeration_items ] .
-- data EnumerationExtension = EnumerationExtension {
--   eETypeRef          :: TypeRef
-- , eEEnumerationItems :: EnumerationItems
-- }
--   deriving (Eq, Show)

-- ' ( ' enumeration_id { ' , ' enumeration_id } ' ) ' .
-- data EnumerationItems = EnumerationItems {
--   eIEnumerationItems :: [EnumerationId]
-- }
--   deriving (Eq, Show)

-- SUBTYPE_CONSTRAINT subtype_constraint_id FOR entity_ref ' ; ' .
-- data SubtypeConstraintHead = SubtypeConstraintHead {
--   sCHSubtypeConstraintId :: SubtypeConstraintId
-- , sCHEntityRef           :: EntityRef
-- }
--   deriving (Eq, Show)

-- [ abstract_supertype ] [ total_over ] [ supertype_expression ' ; ' ] .
-- data SubtypeConstraintBody = SubtypeConstraintBody {
--   sCBAbstractSupertype   :: Maybe AbstractSupertype
-- , sCBTotalOver           :: Maybe TotalOver
-- , sCBSupertypeExpression :: Maybe SupertypeExpression
-- }
--   deriving (Eq, Show)

-- TOTAL_OVER ' ( ' entity_ref { ' , ' entity_ref } ' ) ' ' ; ' .
-- data TotalOver = TotalOver {
--   tOEntityRefs :: [EntityRef]
-- }
--   deriving (Eq, Show)

-- ABSTRACT SUPERTYPE ' ; ' .
-- data AbstractSupertype = AbstractSupertype
--   deriving (Eq, Show)

-- simple_id .
-- type SubtypeConstraintId = SimpleId

-- alias_stmt | assignment_stmt | case_stmt | compound_stmt | escape_stmt | if_stmt | null_stmt | procedure_call_stmt | repeat_stmt | return_stmt | skip_stmt .
-- data Stmt =
--     -- ALIAS variable_id FOR general_ref { qualifier } ' ; ' stmt { stmt } END_ALIAS ' ; ' .
--     AliasStmt {
--       aSVariableId  :: VariableId
--     , alSGeneralRef :: GeneralRef
--     , alSQualifiers :: Maybe [Qualifier]
--     , aSStatements  :: [Stmt]
--     }

--     -- general_ref { qualifier } ' := ' expression ' ; ' .
--   | AssignmentStmt {
--       aSGeneralRef :: GeneralRef
--     , aSQualifiers :: Maybe [Qualifier]
--     , aSExpression :: Expression
--     }

--     -- CASE selector OF { case_action } [ OTHERWISE ' : ' stmt ] END_CASE ' ; ' .
--   | CaseStmt {
--       selector   :: Selector
--     , caseAction :: Maybe [CaseAction]
--     , otherwise  :: Maybe Stmt
--     }

--     -- BEGIN stmt { stmt } END ' ; ' .
--   | CompoundStmt {
--       cSStmts :: [Stmt]
--     }

--     -- ESCAPE ' ; ' .
--   | EscapeStmt

--     -- IF logical_expression THEN stmt { stmt } [ ELSE stmt { stmt } ] END_IF ' ; ' .
--   | IfStmt {
--       ifLogicalExpression :: LogicalExpression
--     , thenStmts           :: [Stmt]
--     , elseStmts           :: Maybe [Stmt]
--     }

--     -- ' ; ' .
--   | NullStmt

--     -- ( built_in_procedure | procedure_ref ) [ actual_parameter_list ] ' ; ' .
--   | ProcedureCallStmt {
--       pCSBody                :: Either BuiltInProcedure ProcedureRef
--     , pCSActualParameterList :: Maybe ActualParameterList
--     }

--     -- REPEAT repeat_control ' ; ' stmt { stmt } END_REPEAT ' ; ' .
--   | RepeatStmt {
--       repeatControl :: RepeatControl
--     , rSStmpts      :: [Stmt]
--     }

--     -- RETURN [ ' ( ' expression ' ) ' ] ' ; ' .
--   | ReturnStmt {
--       rSExpression :: Maybe Expression
--     }

--     -- SKIP ' ; ' .
--   | SkipStmt
--   deriving (Eq, Show)


-- [ increment_control ] [ while_control ] [ until_control ] .
-- data RepeatControl = RepeatControl {
--   incrementControl :: Maybe IncrementControl
-- , whileControl     :: Maybe WhileControl
-- , untilControl     :: Maybe UntilControl
-- }
--   deriving (Eq, Show)

-- UNTIL logical_expression .
-- data UntilControl = UntilControl {
--   logicalExpression :: LogicalExpression
-- }
--   deriving (Eq, Show)

-- WHILE logical_expression .
-- data WhileControl = WhileControl {
--   wCLogicalExpression :: LogicalExpression
-- }
--   deriving (Eq, Show)

-- variable_id ' := ' bound_1 TO bound_2 [ BY increment ] .
-- data IncrementControl = IncrementControl {
--   variableId :: VariableId
-- , bound1     :: Bound1
-- , bound2     :: Bound2
-- , increment  :: Maybe Increment
-- }
--   deriving (Eq, Show)

-- numeric_expression .
-- type Increment = NumericExpression

-- INSERT | REMOVE .
-- data BuiltInProcedure = INSERT | REMOVE
--   deriving (Eq, Show)

-- numeric_expression .
type Width = NumericExpression

-- case_label { ' , ' case_label } ' : ' stmt .
-- data CaseAction = CaseAction {
--   caseLabels :: [CaseLabel]
-- , cAStmt     :: Stmt
-- }
--   deriving (Eq, Show)

-- expression .
-- type CaseLabel = Expression

-- expression .
-- type Selector = Expression

-- { declaration } [ constant_decl ] [ local_decl ] .
-- data AlgorithmHead = AlgorithmHead {
--   aHDeclaration  :: Maybe [Declaration]
-- , aHConstantDecl :: Maybe ConstantDecl
-- , aHLocalDecl    :: Maybe LocalDecl
-- }
--   deriving (Eq, Show)

-- LOCAL local_variable { local_variable } END_LOCAL ' ; ' .
-- data LocalDecl = LocalDecl {
--   localVariables :: [LocalVariable]
-- }
--   deriving (Eq, Show)

-- variable_id { ' , ' variable_id } ' : ' parameter_type [ ' := ' expression ] ' ; ' .
-- data LocalVariable = LocalVariable {
--   variableIds     :: [VariableId]
-- , lVParameterType :: ParameterType
-- , lVExpression    :: Maybe Expression
-- }
--   deriving (Eq, Show)

-- PROCEDURE procedure_id [ ' ( ' [ VAR ] formal_parameter { ' ; ' [ VAR ] formal_parameter } ' ) ' ] ' ; ' .
-- data ProcedureHead = ProcedureHead {
--   procedureId     :: ProcedureId
-- , procedureParams :: Maybe [ProcedureParam]
-- }
--   deriving (Eq, Show)

-- data ProcedureParam = ProcedureParam {
--   -- `modifiable` is True if and only if this parameter was preceded with `VAR`
--   -- in procedure's parameters declaration
--   modifiable      :: Bool
-- , formalParameter :: FormalParameter
-- }
--   deriving (Eq, Show)

-- parameter_id { ' , ' parameter_id } ' : ' parameter_type .
-- data FormalParameter = FormalParameter {
--   fPParameterIds  :: [ParameterId]
-- , fPParameterType :: ParameterType
-- }
--   deriving (Eq, Show)

-- FUNCTION function_id [ ' ( ' formal_parameter { ' ; ' formal_parameter } ' ) ' ] ' : ' parameter_type ' ; ' .
-- data FunctionHead = FunctionHead {
--   fHFunctionId       :: FunctionId
-- , fHFormalParameters :: Maybe [FormalParameter]
-- , fHReturnType       :: ParameterType
-- }
--   deriving (Eq, Show)

-- rel_op | IN | LIKE .
data RelOpExtended =
    OpIN
  | OpLIKE
  -- ' < ' | ' > ' | ' <= ' | ' >= ' | ' <> ' | ' = ' | ' :<>: ' | ' :=: ' .
  | ROL
  | ROG
  | ROLE
  | ROGE
  | RONE
  | ROE
  -- WE is "weird equals", WNE is "weird not equals"
  | ROWNE
  | ROWE
  deriving (Eq, Show)

-- ' ( ' width ' ) ' [ FIXED ] .
data WidthSpec = WidthSpec {
  wsWidth :: Width
, wsFixed :: Bool
}
  deriving (Eq, Show)

-- numeric_expression .
type PrecisionSpec = NumericExpression

-- named_types [ AS ( entity_id | type_id ) ] .
data NamedTypeOrRename = NamedTypeOrRename {
  ntorNamedTypes :: NamedTypes
  -- in the end, both `entity_id` and `type_id` are `simple_id`s, mapping into
  -- `SingleId` type, so we aren't going to distinguish them here either
, ntorAs         :: Maybe SimpleId
}
  deriving (Eq, Show)

-- rule_head algorithm_head { stmt } where_clause END_RULE ' ; ' .
-- data RuleDecl = RuleDecl {
--   ruleHead        :: RuleHead
-- , rDAlgorithmHead :: AlgorithmHead
-- , rDStmts         :: Maybe [Stmt]
-- , rDWhereClause   :: WhereClause
-- }
--   deriving (Eq, Show)

-- RULE rule_id FOR ' ( ' entity_ref { ' , ' entity_ref } ' ) ' ' ; ' .
-- data RuleHead = RuleHead {
--   ruleId     :: RuleId
-- , entityRefs :: [EntityRef]
-- }
--   deriving (Eq, Show)

-- simple_id .
-- type RuleId = SimpleId

