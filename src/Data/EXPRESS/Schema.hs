module Schema where

{- Lists are implied to be one or more elements long. If list can be empty, we
 - use Maybe [...] instead.
 -}

import Data.Text (Text)

-- schema_decl { schema_decl } .
data Express = Express [Schema]

-- SCHEMA schema_id [ schema_version_id ] ' ; ' schema_body END_SCHEMA ' ; ' .
data Schema = Schema {
  id      :: SchemaId
, version :: Maybe SchemaVersionId
, sBody   :: SchemaBody
}

-- simple_id .
type SchemaId = SimpleId

-- string_literal .
type SchemaVersionId = StringLiteral

-- { interface_specification } [ constant_decl ] { declaration | rule_decl } .
data SchemaBody = SchemaBody {
  interfaces    :: Maybe [InterfaceSpecification]
, constant      :: Maybe ConstantDecl
, declaractions :: Maybe [Declaration]
, rules         :: Maybe [RuleDecl]
}

-- letter { letter | digit | ' _ ' } .
type SimpleId = Text

{- \q { ( \q \q ) | not_quote | \s | \x9 | \xA | \xD } \q .

     or

   ' " ' encoded_character { encoded_character } ' " ' . -}
type StringLiteral = Text

-- reference_clause | use_clause .
class InterfaceSpecification a
instance InterfaceSpecification ReferenceClause
instance InterfaceSpecification UseClause

-- REFERENCE FROM schema_ref [ ' ( ' resource_or_rename { ' , ' resource_or_rename } ' ) ' ] ' ; ' .
data ReferenceClause = ReferenceClause {
  schemaRef :: SchemaRef
, resources :: Maybe [ResourceOrRename]
}

-- schema_id .
type SchemaRef = SchemaId

-- resource_ref [ AS rename_id ] .
data ResourceOrRename = ResourceOrRename {
  resourceRef :: ResourceRef
, renameId    :: Maybe RenameId
}

-- constant_ref | entity_ref | function_ref | procedure_ref | type_ref .
class ResourceRef a
instance ResourceRef ConstantRef
instance ResourceRef EntityRef
instance ResourceRef FunctionRef
instance ResourceRef ProcedureRef
instance ResourceRef TypeRef

-- constant_id .
type ConstantRef = ConstantId

-- entity_id .
type EntityRef = EntityId

-- function_id .
type FunctionRef = FunctionId

-- procedure_id .
type ProcedureRef = ProcedureId

-- type_id .
type TypeRef = TypeId

-- constant_id | entity_id | function_id | procedure_id | type_id .
class RenameId a
instance RenameId ConstantId
instance RenameId EntityId
instance RenameId FunctionId
instance RenameId ProcedureId
instance RenameId TypeId

-- simple_id .
type ConstantId = SimpleId

-- simple_id .
type EntityId = SimpleId

-- simple_id .
type FunctionId = SimpleId

-- simple_id .
type ProcedureId = SimpleId

-- simple_id .
type TypeId = SimpleId

-- CONSTANT constant_body { constant_body } END_CONSTANT ' ; ' .
data ConstantDecl = ConstantDecl {
  cbody :: Maybe [ConstantBody]
}

-- constant_id ' : ' instantiable_type ' := ' expression ' ; ' .
data ConstantBody = ConstantBody {
  cid          :: ConstantId
, type_        :: InstantiableType
, cBExpression :: Expression
}

-- concrete_types | entity_ref .
class InstantiableType a
instance ConcreteTypes a => InstantiableType a
instance InstantiableType EntityRef

-- aggregation_types | simple_types | type_ref .
class ConcreteTypes a
instance AggregationTypes a => ConcreteTypes a
instance SimpleTypes a => ConcreteTypes a
instance ConcreteTypes TypeRef

-- array_type | bag_type | list_type | set_type .
class AggregationTypes a
instance AggregationTypes (ArrayType b)
instance AggregationTypes (BagType b)
instance AggregationTypes (ListType b)
instance AggregationTypes (SetType b)

-- binary_type | boolean_type | integer_type | logical_type | number_type | real_type | string_type .
class SimpleTypes a
instance SimpleTypes BinaryType
instance SimpleTypes BooleanType
instance SimpleTypes IntegerType
instance SimpleTypes LogicalType
instance SimpleTypes NumberType
instance SimpleTypes RealType
instance SimpleTypes StringType

-- simple_expression [ rel_op_extended simple_expression ] .
data Expression where
  ESimple :: SimpleExpression -> Expression
  EOp :: SimpleExpression -> RelOpExtended -> SimpleExpression -> Expression

-- entity_decl | function_decl | procedure_decl | subtype_constraint_decl | type_decl .
class Declaration a
instance Declaration EntityDecl
instance Declaration FunctionDecl
instance Declaration ProcedureDecl
instance Declaration SubtypeConstraintDecl
instance Declaration TypeDecl

-- entity_head entity_body END_ENTITY ' ; ' .
data EntityDecl = EntityDecl {
  ehead :: EntityHead
, ebody :: EntityBody
}

-- ENTITY entity_id subsuper ' ; ' .
data EntityHead = EntityHead {
  eid      :: EntityId
, subsuper :: SubSuper
}

-- { explicit_attr } [ derive_clause ] [ inverse_clause ] [ unique_clause ] [ where_clause ] .
data EntityBody = EntityBody {
  eBAttrs       :: Maybe [ExplicitAttr]
, deriveClause  :: Maybe DeriveClause
, inverseClause :: Maybe InverseClause
, uniqueClause  :: Maybe UniqueClause
, eBWhereClause :: Maybe WhereClause
}

-- [ supertype_constraint ] [ subtype_declaration ] .
data SubSuper = SubSuper {
  sup :: Maybe SupertypeConstraint
, sub :: Maybe SubtypeDeclaration
}

-- abstract_entity_declaration | abstract_supertype_declaration | supertype_rule .
class SupertypeConstraint a
instance SupertypeConstraint AbstractEntityDeclaration
instance SupertypeConstraint AbstractSupertypeDeclaration
instance SupertypeConstraint SupertypeRule

-- ABSTRACT .
data AbstractEntityDeclaration = AbstractEntityDeclaration

-- ABSTRACT SUPERTYPE [ subtype_constraint ] .
data AbstractSupertypeDeclaration = AbstractSupertypeDeclaration {
  subtypeConstraint :: Maybe SubtypeConstraint
}

-- SUPERTYPE subtype_constraint .
data SupertypeRule = SupertypeRule {
  srSubtypeConstraint :: SubtypeConstraint
}

-- SUBTYPE OF ' ( ' entity_ref { ' , ' entity_ref } ' ) ' .
data SubtypeDeclaration = SubtypeDeclaration {
  subtypeOf :: [EntityRef]
}

-- OF ' ( ' supertype_expression ' ) ' .
data SubtypeConstraint = SubtypeConstraint {
  scSupertypeExpression :: SupertypeExpression
}

-- supertype_factor { ANDOR supertype_factor } .
data SupertypeExpression where
  SEFactor :: SupertypeFactor -> SupertypeExpression
  SEAndOr :: SupertypeFactor -> SupertypeFactor -> SupertypeExpression

-- supertype_term { AND supertype_term } .
data SupertypeFactor where
  STTerm :: SupertypeTerm -> SupertypeFactor
  And :: SupertypeTerm -> SupertypeTerm -> SupertypeFactor

-- entity_ref | one_of | ' ( ' supertype_expression ' ) ' .
class SupertypeTerm a
instance SupertypeTerm EntityRef
instance SupertypeTerm OneOf
instance SupertypeTerm SupertypeExpression

-- ONEOF ' ( ' supertype_expression { ' , ' supertype_expression } ' ) ' .
data OneOf = OneOf {
  oOExpressions :: [SupertypeExpression]
}

-- WHERE domain_rule ' ; ' { domain_rule ' ; ' } .
data WhereClause = WhereClause {
  domainRules :: [DomainRule]
}

-- [ rule_label_id ' : ' ] expression .
data DomainRule = DomainRule {
  dRRuleLabelId :: Maybe RuleLabelId
, expr          :: Expression
}

-- simple_id .
type RuleLabelId = SimpleId

-- UNIQUE unique_rule ' ; ' { unique_rule ' ; ' } .
data UniqueClause = UniqueClause {
  uniqueRule :: [UniqueRule]
}

-- [ rule_label_id ' : ' ] referenced_attribute { ' , ' referenced_attribute } .
data UniqueRule = UniqueRule {
  ruleLabelId :: Maybe RuleLabelId
, attrs       :: [ReferencedAttribute]
}

-- attribute_ref | qualified_attribute .
class ReferencedAttribute
instance ReferencedAttribute AttributeRef
instance ReferencedAttribute QualifiedAttribute

-- SELF group_qualifier attribute_qualifier .
data QualifiedAttribute = QualifiedAttribute {
  groupQualifier     :: GroupQualifier
, attributeQualifier :: AttributeQualifier
}

-- ' . ' attribute_ref .
data AttributeQualifier = AttributeQualifier {
  aQAttributeRef :: AttributeRef
}

-- attribute_id .
type AttributeRef = AttributeId

-- simple_id .
type AttributeId = SimpleId

-- ' \ ' entity_ref .
data GroupQualifier = GroupQualifier {
  gQEntityRef :: EntityRef
}

-- INVERSE inverse_attr { inverse_attr } .
data InverseClause = InverseClause {
  inverseAttrs :: [InverseAttr]
}

-- attribute_decl ' : ' [ ( SET | BAG ) [ bound_spec ] OF ] entity_ref FOR [ entity_ref ' . ' ] attribute_ref ' ; ' .
data InverseAttr = InverseAttr {
  iAAttributeDecl :: AttributeDecl
, setOrBagBounds  :: Maybe SetOrBagBounds
, iAEntityRef     :: EntityRef
, forEntityRef    :: Maybe EntityRef
, attributeRef    :: AttributeRef
}

data SetOrBagBounds = SetOrBagBounds {
  setOrBag   :: SetOrBag
, sOBBBounds :: Maybe BoundSpec
}

data SetOrBag = SetTag | BagTag

-- ' [ ' bound_1 ' : ' bound_2 ' ] ' .
data BoundSpec = BoundSpec {
  bSBound1 :: Bound1
, bSBound2 :: Bound2
}

-- numeric_expression .
type Bound1 = NumericExpression

-- numeric_expression .
type Bound2 = NumericExpression

-- simple_expression .
type NumericExpression = SimpleExpression

-- term { add_like_op term } .
data SimpleExpression where
  SETerm :: Term -> SimpleExpression
  SEAddLikeOp :: Term -> AddLikeOp -> Term -> SimpleExpression

-- factor { multiplication_like_op factor } .
data Term where
  TFactor :: Factor -> Term
  TMultiplicationLikeOp :: Factor -> MultiplicationLikeOp -> Factor -> Term

-- simple_factor [ ' ** ' simple_factor ] .
data Factor where
  FSimpleFactor :: SimpleFactor -> Factor
  FSPow :: SimpleFactor -> SimpleFactor -> Factor

-- aggregate_initializer | entity_constructor | enumeration_reference | interval | query_expression | ( [ unary_op ] ( ' ( ' expression ' ) ' | primary ) ) .
class SimpleFactor a
instance SimpleFactor AggregateInitializer
instance SimpleFactor EntityConstructor
instance SimpleFactor EnumerationReference
instance SimpleFactor Interval
instance SimpleFactor QueryExpression
instance SimpleFactor UnaryOppedSF

data UnaryOppedSF = UnaryOppedSF {
  unaryOp  :: Maybe UnaryOp
, uOSFBody :: Either Expression Primary
}

-- literal | ( qualifiable_factor { qualifier } ) .
class Primary a
instance Primary Literal
instance Primary QualifiableFactorWithQualifiers

data QualifiableFactorWithQualifiers = QualifiableFactorWithQualifiers {
  qfwqFactor :: QualifiableFactor
, qualifier  :: Maybe [Qualifier]
}

-- attribute_qualifier | group_qualifier | index_qualifier .
class Qualifier a
instance Qualifier AttributeQualifier
instance Qualifier GroupQualifier
instance Qualifier IndexQualifier

-- ' [ ' index_1 [ ' : ' index_2 ] ' ] ' .
data IndexQualifier = IndexQualifier {
  index1 :: Index1
, index2 :: Maybe Index2
}

-- index .
type Index1 = Index

-- index .
type Index2 = Index

-- numeric_expression .
type Index = NumericExpression

-- attribute_ref | constant_factor | function_call | general_ref | population .
class QualifiableFactor a
instance QualifiableFactor AttributeRef
instance QualifiableFactor ConstantFactor
instance QualifiableFactor FunctionCall
instance QualifiableFactor GeneralRef
instance QualifiableFactor Population

-- built_in_constant | constant_ref .
class ConstantFactor a
instance ConstantFactor BuiltInConstant
instance ConstantFactor ConstantRef

-- ( built_in_function | function_ref ) [ actual_parameter_list ] .
data FunctionCall = FunctionCall {
  begin :: Either BuiltInFunction FunctionRef
, params :: Maybe ActualParameterList
}

-- parameter_ref | variable_ref .
class GeneralRef a
instance GeneralRef ParameterRef
instance GeneralRef VariableRef

-- entity_ref .
type Population = EntityRef

-- variable_id .
type VariableRef = VariableId

-- simple_id .
type VariableId = SimpleId

-- parameter_id .
type ParameterRef = ParameterId

-- simple_id .
type ParameterId = SimpleId

-- ' ( ' parameter { ' , ' parameter } ' ) ' .
data ActualParameterList = ActualParameterList {
  parameters :: [Parameter]
}

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

-- CONST_E | PI | SELF | ' ? ' .
data BuiltInConstant = CONST_E | PI | SELF | QuestionMark

-- binary_literal | logical_literal | real_literal | string_literal .
class Literal a
instance Literal BinaryLiteral
instance Literal LogicalLiteral
instance Literal RealLiteral
instance Literal StringLiteral

-- nteger_literal | ( digits ' . ' [ digits ] [ ' e ' [ sign ] digits ] ) .
data RealLiteral = RealLiteral

-- FALSE | TRUE | UNKNOWN .
data LogicalLiteral = FALSE | TRUE | UNKNOWN

-- ' % ' bit { bit } .
data BinaryLiteral = BinaryLiteral {
  bits :: [Bit]
}

-- ' 0 ' | ' 1 ' .
data Bit = Zero | One

-- ' + ' | ' - ' | NOT .
data UnaryOp = UPlus | UMinus | UNOT

-- QUERY ' ( ' variable_id ' <* ' aggregate_source ' | ' logical_expression ' ) ' .
data QueryExpression = QueryExpression {
  qEVariableId        :: VariableId
, aggregateSource     :: AggregateSource
, qELogicalExpression :: LogicalExpression
}

-- expression .
type LogicalExpression = Expression

-- simple_expression .
type AggregateSource = SimpleExpression

-- ' { ' interval_low interval_op interval_item interval_op interval_high ' } ' .
data Interval = Interval {
  intervalLow  :: IntervalLow
, intervalOp   :: IntervalOp
, intervalItem :: IntervalItem
, intervalOp2  :: IntervalOp
, intervalHigh :: IntervalHigh
}

-- simple_expression .
type IntervalLow = SimpleExpression

-- simple_expression .
type IntervalHigh = SimpleExpression

-- simple_expression .
type IntervalItem = SimpleExpression

-- ' < ' | ' <= ' .
data IntervalOp = Less | LessEqual

-- [ type_ref ' . ' ] enumeration_ref .
data EnumerationReference = EnumerationReference {
  typeRef        :: Maybe TypeRef
, enumerationRef :: EnumerationRef
}

-- enumeration_id .
type EnumerationRef = EnumerationId

-- simple_id .
type EnumerationId = SimpleId

-- entity_ref ' ( ' [ expression { ' , ' expression } ] ' ) ' .
data EntityConstructor = EntityConstructor {
  entityRef   :: EntityRef
, expressions :: Maybe [Expression]
}

-- ' [ ' [ element { ' , ' element } ] ' ] ' .
data AggregateInitializer = AggregateInitializer {
  elements :: Maybe [Element]
}

-- expression [ ' : ' repetition ] .
data Element = Element {
  eExpression :: Expression
, repetition  :: Maybe Repetition
}

-- numeric_expression .
type Repetition = NumericExpression

-- ' * ' | ' / ' | DIV | MOD | AND | ' || ' .
-- `Or` is `||`
data MultiplicationLikeOp = Times | Divide | DIV | MOD | AND | Or

-- ' + ' | ' - ' | OR | XOR .
data AddLikeOp = Plus | Minus | OR | XOR

-- attribute_id | redeclared_attribute .
class AttributeDecl a
instance AttributeDecl AttributeId
instance AttributeDecl RedeclaredAttribute

-- qualified_attribute [ RENAMED attribute_id ] .
data RedeclaredAttribute = RedeclaredAttribute {
  qualifiedAttribute :: QualifiedAttribute
, attributeId        :: Maybe AttributeId
}

-- DERIVE derived_attr { derived_attr } .
data DeriveClause = DeriveClause {
  derivedAttrs :: [DerivedAttr]
}

-- attribute_decl ' : ' parameter_type ' := ' expression ' ; ' .
data DerivedAttr = DerivedAttr {
  attributeDecl   :: AttributeDecl
, dAParameterType :: ParameterType
, expression      :: Expression
}

-- generalized_types | named_types | simple_types .
class ParameterType a
instance GeneralizedTypes a => ParameterType a
instance NamedTypes a => ParameterType a
instance SimpleTypes a => ParameterType a

-- entity_ref | type_ref .
class NamedTypes a
instance NamedTypes EntityRef
instance NamedTypes TypeRef

-- aggregate_type | general_aggregation_types | generic_entity_type | generic_type .
class GeneralizedTypes a
instance GeneralizedTypes AggregateType
instance GeneralAggregationTypes a => GeneralizedTypes
instance GeneralizedTypes GenericEntityType
instance GeneralizedTypes GenericType

-- GENERIC [ ' : ' type_label ] .
data GenericType = GenericType {
  gTTypeLabel :: Maybe TypeLabel
}

-- type_label_id | type_label_ref .
class TypeLabel a
instance TypeLabel TypeLabelId
instance TypeLabel TypeLabelRef

-- type_label_id .
type TypeLabelRef = TypeLabelId

-- simple_id .
type TypeLabelId = SimpleId

-- GENERIC_ENTITY [ ' : ' type_label ] .
data GenericEntityType = GenericEntityType {
  gETTypeLabel :: Maybe TypeLabel
}

-- general_array_type | general_bag_type | general_list_type | general_set_type .
class GeneralAggregationTypes a
instance GeneralAggregationTypes GeneralArrayType
instance GeneralAggregationTypes GeneralBagType
instance GeneralAggregationTypes GeneralListType
instance GeneralAggregationTypes GeneralSetType

-- ARRAY [ bound_spec ] OF [ OPTIONAL ] [ UNIQUE ] parameter_type .
data GeneralArrayType = GeneralArrayType {
  gATBounds        :: Maybe BoundSpec
, optional         :: Bool
, gATUnique        :: Bool
, gATParameterType :: ParameterType
}

-- BAG [ bound_spec ] OF parameter_type .
data GeneralBagType = GeneralBagType {
  gBTBounds        :: Maybe BoundSpec
, gBTParameterType :: ParameterType
}

-- LIST [ bound_spec ] OF [ UNIQUE ] parameter_type .
data GeneralListType = GeneralListType {
  gLTBounds        :: Maybe BoundSpec
, unique           :: Bool
, gLTParameterType :: ParameterType
}

-- SET [ bound_spec ] OF parameter_type .
data GeneralSetType = GeneralSetType {
  bounds        :: Maybe BoundSpec
, parameterType :: ParameterType
}

-- AGGREGATE [ ' : ' type_label ] OF parameter_type .
data AggregateType = AggregateType {
  typeLabel       :: Maybe TypeLabel
, aTParameterType :: ParameterType
}

-- attribute_decl { ' , ' attribute_decl } ' : ' [ OPTIONAL ] parameter_type ' ; ' .
data ExplicitAttr = ExplicitAttr {
  attributeDecls  :: [AttributeDecl]
, eAOptional      :: Bool
, eAParameterType :: ParameterType
}

-- TYPE type_id ' = ' underlying_type ' ; ' [ where_clause ] END_TYPE ' ; ' .
data TypeDecl = TypeDecl {
  tDTypeId       :: TypeId
, underlyingType :: UnderlyingType
, whereClause    :: Maybe WhereClause
}

-- concrete_types | constructed_types .
class UnderlyingType a
instance ConcreteTypes a => UnderlyingType a
instance ConstructedTypes a => UnderlyingType a

-- enumeration_type | select_type .
class ConstructedTypes a
instance ConstructedTypes EnumerationType
instance ConstructedTypes SelectType

-- [ EXTENSIBLE [ GENERIC_ENTITY ] ] SELECT [ select_list | select_extension ] .
data SelectType = SelectType {
  extensibleGenericEntity :: Maybe (Extensible (Maybe GenericEntity))
, sTBody                  :: Maybe (Either SelectList SelectExtension)
}

data Extensible a = Extensible a

data GenericEntity = GenericEntity

-- BASED_ON type_ref [ WITH select_list ] .
data SelectExtension = SelectExtension {
  sETypeRef    :: TypeRef
, sESelectList :: Maybe SelectList
}

-- ' ( ' named_types { ' , ' named_types } ' ) ' .
data SelectList = SelectList {
  sLNamedTypes :: [NamedTypes]
}

-- [ EXTENSIBLE ] ENUMERATION [ ( OF enumeration_items ) | enumeration_extension ] .
data EnumerationType = EnumerationType {
  extensible :: Bool
, body       :: Maybe (Either EnumerationItems EnumerationExtension)
}

-- BASED_ON type_ref [ WITH enumeration_items ] .
data EnumerationExtension = EnumerationExtension {
  eETypeRef          :: TypeRef
, eEEnumerationItems :: EnumerationItems
}

-- ' ( ' enumeration_id { ' , ' enumeration_id } ' ) ' .
data EnumerationItems = EnumerationItems {
  eIEnumerationItems :: [EnumerationId]
}

-- subtype_constraint_head subtype_constraint_body END_SUBTYPE_CONSTRAINT ' ; ' .
data SubtypeConstraintDecl = SubtypeConstraintDecl {
  subtypeConstraintHead :: SubtypeConstraintHead
, subtypeConstraintBody :: SubtypeConstraintBody
}

-- SUBTYPE_CONSTRAINT subtype_constraint_id FOR entity_ref ' ; ' .
data SubtypeConstraintHead = SubtypeConstraintHead {
  sCHSubtypeConstraintId :: SubtypeConstraintId
, sCHEntityRef           :: EntityRef
}

-- [ abstract_supertype ] [ total_over ] [ supertype_expression ' ; ' ] .
data SubtypeConstraintBody = SubtypeConstraintBody {
  sCBAbstractSupertype   :: Maybe AbstractSupertype
, sCBTotalOver           :: Maybe TotalOver
, sCBSupertypeExpression :: Maybe SupertypeExpression
}

-- TOTAL_OVER ' ( ' entity_ref { ' , ' entity_ref } ' ) ' ' ; ' .
data TotalOver = TotalOver {
  tOEntityRefs :: [EntityRef]
}

-- ABSTRACT SUPERTYPE ' ; ' .
data AbstractSupertype = AbstractSupertype

-- simple_id .
type SubtypeConstraintId = SimpleId

-- procedure_head algorithm_head { stmt } END_PROCEDURE ' ; ' .
data ProcedureDecl = ProcedureDecl {
  procedureHead :: ProcedureHead
, algorithmHead :: AlgorithmHead
, stmts         :: Maybe [Stmt]
}

-- alias_stmt | assignment_stmt | case_stmt | compound_stmt | escape_stmt | if_stmt | null_stmt | procedure_call_stmt | repeat_stmt | return_stmt | skip_stmt .
data Stmt =
    -- ALIAS variable_id FOR general_ref { qualifier } ' ; ' stmt { stmt } END_ALIAS ' ; ' .
    AliasStmt {
      aSVariableId  :: VariableId
    , alSGeneralRef :: GeneralRef
    , alSQualifiers :: Maybe [Qualifier]
    , aSStatements  :: [Stmt]
    }

    -- general_ref { qualifier } ' := ' expression ' ; ' .
  | AssignmentStmt {
      aSGeneralRef :: GeneralRef
    , aSQualifiers :: Maybe [Qualifier]
    , aSExpression :: Expression
    }

    -- CASE selector OF { case_action } [ OTHERWISE ' : ' stmt ] END_CASE ' ; ' .
  | CaseStmt {
      selector   :: Selector
    , caseAction :: Maybe [CaseAction]
    , otherwise  :: Maybe Stmt
    }

    -- BEGIN stmt { stmt } END ' ; ' .
  | CompoundStmt {
      cSStmts :: [Stmt]
    }

    -- ESCAPE ' ; ' .
  | EscapeStmt

    -- IF logical_expression THEN stmt { stmt } [ ELSE stmt { stmt } ] END_IF ' ; ' .
  | IfStmt {
      ifLogicalExpression :: LogicalExpression
    , thenStmts           :: [Stmt]
    , elseStmts           :: Maybe [Stmt]
    }

    -- ' ; ' .
  | NullStmt

    -- ( built_in_procedure | procedure_ref ) [ actual_parameter_list ] ' ; ' .
  | ProcedureCallStmt {
      pCSBody                :: Either BuiltInProcedure ProcedureRef
    , pCSActualParameterList :: Maybe ActualParameterList
    }

    -- REPEAT repeat_control ' ; ' stmt { stmt } END_REPEAT ' ; ' .
  | RepeatStmt {
      repeatControl :: RepeatControl
    , rSStmpts      :: [Stmt]
    }

    -- RETURN [ ' ( ' expression ' ) ' ] ' ; ' .
  | ReturnStmt {
      rSExpression :: Maybe Expression
    }

    -- SKIP ' ; ' .
  | SkipStmt


-- [ increment_control ] [ while_control ] [ until_control ] .
data RepeatControl = RepeatControl {
  incrementControl :: Maybe IncrementControl
, whileControl     :: Maybe WhileControl
, untilControl     :: Maybe UntilControl
}

-- UNTIL logical_expression .
data UntilControl = UntilControl {
  logicalExpression :: LogicalExpression
}

-- WHILE logical_expression .
data WhileControl = WhileControl {
  wCLogicalExpression :: LogicalExpression
}

-- variable_id ' := ' bound_1 TO bound_2 [ BY increment ] .
data IncrementControl = IncrementControl {
  variableId :: VariableId
, bound1     :: Bound1
, bound2     :: Bound2
, increment  :: Maybe Increment
}

-- numeric_expression .
type Increment = NumericExpression

-- INSERT | REMOVE .
data BuiltInProcedure = INSERT | REMOVE

-- numeric_expression .
type Width = NumericExpression

-- case_label { ' , ' case_label } ' : ' stmt .
data CaseAction = CaseAction {
  caseLabels :: [CaseLabel]
, cAStmt     :: Stmt
}

-- expression .
type CaseLabel = Expression

-- expression .
type Selector = Expression

-- { declaration } [ constant_decl ] [ local_decl ] .
data AlgorithmHead = AlgorithmHead {
  aHDeclaration  :: Maybe [Declaration]
, aHConstantDecl :: Maybe ConstantDecl
, aHLocalDecl    :: Maybe LocalDecl
}

-- LOCAL local_variable { local_variable } END_LOCAL ' ; ' .
data LocalDecl = LocalDecl {
  localVariables :: [LocalVariable]
}

-- variable_id { ' , ' variable_id } ' : ' parameter_type [ ' := ' expression ] ' ; ' .
data LocalVariable = LocalVariable {
  variableIds     :: [VariableId]
, lVParameterType :: ParameterType
, lVExpression    :: Maybe Expression
}

-- PROCEDURE procedure_id [ ' ( ' [ VAR ] formal_parameter { ' ; ' [ VAR ] formal_parameter } ' ) ' ] ' ; ' .
data ProcedureHead = ProcedureHead {
  procedureId     :: ProcedureId
, procedureParams :: Maybe [ProcedureParam]
}

data ProcedureParam = ProcedureParam {
  -- `modifiable` is True if and only if this parameter was preceded with `VAR`
  -- in procedure's parameters declaration
  modifiable      :: Bool
, formalParameter :: FormalParameter
}

-- parameter_id { ' , ' parameter_id } ' : ' parameter_type .
data FormalParameter = FormalParameter {
  fPParameterIds  :: [ParameterId]
, fPParameterType :: ParameterType
}

-- function_head algorithm_head stmt { stmt } END_FUNCTION ' ; ' .
data FunctionDecl = FunctionDecl {
  fDFunctionHead  :: FunctionHead
, fDAlgorithmHead :: AlgorithmHead
, fDStatements    :: [Stmt]
}

-- FUNCTION function_id [ ' ( ' formal_parameter { ' ; ' formal_parameter } ' ) ' ] ' : ' parameter_type ' ; ' .
data FunctionHead = FunctionHead {
  fHFunctionId       :: FunctionId
, fHFormalParameters :: Maybe [FormalParameter]
, fHReturnType       :: ParameterType
}

-- rel_op | IN | LIKE .
class RelOpExtended a
instance RelOpExtended RelOp
instance RelOpExtended OpIN
instance RelOpExtended OpLIKE

data OpIN = OpIN

data OpLIKE = OpLIKE

-- ' < ' | ' > ' | ' <= ' | ' >= ' | ' <> ' | ' = ' | ' :<>: ' | ' :=: ' .
data RelOp = ROL | ROG | ROLE | ROGE | RONE | ROE | ROWNE | ROWE
-- WE is "weird equals", WNE is "weird not equals"

-- STRING [ width_spec ] .
data StringType = StringType {
  widthSpec :: Maybe WidthSpec
}

-- ' ( ' width ' ) ' [ FIXED ] .
data WidthSpec = WidthSpec {
  width :: Width
, fixed :: Bool
}

-- REAL [ ' ( ' precision_spec ' ) ' ] .
data RealType = RealType {
  precisionSpec :: Maybe PrecisionSpec
}

-- numeric_expression .
type PrecisionSpec = NumericExpression

-- NUMBER .
data NumberType = NumberType

-- LOGICAL .
data LogicalType = LogicalType

-- INTEGER .
data IntegerType = IntegerType

-- BOOLEAN .
data BooleanType = BooleanType

-- BINARY [ width_spec ] .
data BinaryType = BinaryType {
  bTWidthSpec :: Maybe WidthSpec
}

-- SET [ bound_spec ] OF instantiable_type .
data SetType = SetType {
  sTBounds :: Maybe BoundSpec
, sTType   :: InstantiableType
}

-- LIST [ bound_spec ] OF [ UNIQUE ] instantiable_type .
data ListType = ListType {
  lTBoundSpec :: Maybe BoundSpec
, lTUnique    :: Bool
, lTType      :: InstantiableType
}

-- BAG [ bound_spec ] OF instantiable_type .
data BagType = BagType {
  bTBoundSpec :: Maybe BoundSpec
, bTType      :: InstantiableType
}

-- ARRAY bound_spec OF [ OPTIONAL ] [ UNIQUE ] instantiable_type .
data ArrayType = ArrayType {
  aTBounds   :: BoundSpec
, aTOptional :: Bool
, aTUnique   :: Bool
, aTType     :: InstantiableType
}

-- USE FROM schema_ref [ ' ( ' named_type_or_rename { ' , ' named_type_or_rename } ' ) ' ] ' ; ' .
data UseClause = UseClause {
  uCSchemaRef         :: SchemaRef
, namedTypesOrRenames :: Maybe [NamedTypeOrRename]
}

-- named_types [ AS ( entity_id | type_id ) ] .
data NamedTypeOrRename = NamedTypeOrRename {
  nTORNamedTypes :: NamedTypes
, as             :: Maybe (Either EntityId TypeId)
}

-- rule_head algorithm_head { stmt } where_clause END_RULE ' ; ' .
data RuleDecl = RuleDecl {
  ruleHead        :: RuleHead
, rDAlgorithmHead :: AlgorithmHead
, rDStmts         :: Maybe [Stmt]
, rDWhereClause   :: WhereClause
}

-- RULE rule_id FOR ' ( ' entity_ref { ' , ' entity_ref } ' ) ' ' ; ' .
data RuleHead = RuleHead {
  ruleId     :: RuleId
, entityRefs :: [EntityRef]
}

-- simple_id .
type RuleId = SimpleId

