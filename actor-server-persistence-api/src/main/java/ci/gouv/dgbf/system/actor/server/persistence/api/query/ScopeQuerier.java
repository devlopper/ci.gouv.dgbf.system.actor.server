package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.exists;
import static org.cyk.utility.persistence.query.Language.Where.like;
import static org.cyk.utility.persistence.query.Language.Where.not;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Language.From;
import org.cyk.utility.persistence.query.Language.Order;
import org.cyk.utility.persistence.query.Language.Select;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ScopeQuerier extends Querier.CodableAndNamable<Scope> {

	String PARAMETER_NAME_TYPE = "type";
	String PARAMETER_NAME_TYPE_CODE = "typeCode";
	String PARAMETER_NAME_TYPE_IDENTIFIER = "typeIdentifier";
	String PARAMETER_NAME_FUNCTION_IDENTIFIER = "functionIdentifier";
	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	String PARAMETER_NAME_TYPE_NAME = "typeName";
	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_TYPES_IDENTIFIERS = "typesIdentifiers";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_SECTION_CODE_NAME = "sectionCodeName";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "budgetSpecializationUnitCodeName";
	String PARAMETER_NAME_ACTION_CODE_NAME = "actionCodeName";
	String PARAMETER_NAME_ACTIVITY_CODE_NAME = "activityCodeName";
	String PARAMETER_NAME_CATEGORY_CODE_NAME = "categoryCodeName";
	String PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME = "economicNatureCodeName";
	
	String PARAMETER_NAME_VISIBLE = "visible";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_TYPE_NAME = 4;
	
	String FLAG_VISIBLE = "Scope.visible";
	String FLAG_INVISIBLE = "Scope.invisible";
	String FLAG_LIST = "Scope.list";
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Scope.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(Scope.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Scope.class, QueryName.COUNT_DYNAMIC);
	
	/* read where filter order by type code ascending by code ascending */
	String QUERY_NAME_READ_WHERE_FILTER = "readWhereFilter";
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_FILTER);
	String QUERY_VALUE_READ_WHERE_FILTER_FROM_WHERE = Language.From.of("Scope t")+" "+Language.Where.of(Language.Where.and(
		Language.Where.like("t", "type.code", PARAMETER_NAME_TYPE_CODE)
		,Language.Where.like("t", "type.name", PARAMETER_NAME_TYPE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_TYPE_NAME)
		,Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
		,Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
		,"(:"+PARAMETER_NAME_ACTOR_CODE_NULLABLE+" = true OR EXISTS(SELECT t0 FROM ActorScope t0 WHERE t0.scope = t AND "+Language.Where.like("t0", "actor.code", PARAMETER_NAME_ACTOR_CODE)+"))"
		));
	String QUERY_VALUE_READ_WHERE_FILTER = Language.of(Language.Select.of("t.identifier,t.code,t.name"),QUERY_VALUE_READ_WHERE_FILTER_FROM_WHERE,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_NAME_COUNT_WHERE_FILTER = "countWhereFilter";
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_FILTER);
	String QUERY_VALUE_COUNT_WHERE_FILTER = Language.of(Language.Select.of("COUNT(t.identifier)"),QUERY_VALUE_READ_WHERE_FILTER_FROM_WHERE);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/* read where type is ua and filter order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_UA_AND_FILTER = "readWhereTypeIsUAAndFilter";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_TYPE_IS_UA_AND_FILTER);
	Map<String,Integer> QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE
			,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE = " FROM Scope t "
			+ " JOIN AdministrativeUnit administrativeUnit ON administrativeUnit.identifier = t.identifier "
			+ " JOIN Section section ON section.identifier = administrativeUnit.section "
			+ " JOIN Scope sectionScope ON sectionScope.identifier = section.identifier "
			+Language.Where.of(Language.Where.and(				
					Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE),Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				));
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER = "SELECT t.identifier,t.code,t.name,"+Language.Select.concatCodeName("sectionScope")+ QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE+ " ORDER BY t.code ASC";
	Collection<Scope> readWhereTypeIsUAAndFilter(QueryExecutorArguments arguments);
	
	/* count where type is ua and filter */
	String QUERY_NAME_COUNT_WHERE_TYPE_IS_UA_AND_FILTER = "countWhereTypeIsUAAndFilter";
	String QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_TYPE_IS_UA_AND_FILTER);
	String QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA_AND_FILTER = "SELECT COUNT(t.identifier) "+QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE;
	Long countWhereTypeIsUAAndFilter(QueryExecutorArguments arguments);
	
	/* read where type is usb and filter order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_USB_AND_FILTER = "readWhereTypeIsUSBAndFilter";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_TYPE_IS_USB_AND_FILTER);
	Map<String,Integer> QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE
			,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_WHERE_TYPE_IS_USB_AND_FILTER_FROM_WHERE = " FROM Scope t "
			+ " JOIN BudgetSpecializationUnit budgetSpecializationUnit ON budgetSpecializationUnit.identifier = t.identifier "
			+ " JOIN Section section ON section.identifier = budgetSpecializationUnit.section "
			+ " JOIN Scope sectionScope ON sectionScope.identifier = section.identifier "
			+Language.Where.of(Language.Where.and(				
					Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE),Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				));
	String QUERY_VALUE_READ_WHERE_TYPE_IS_USB_AND_FILTER = "SELECT t.identifier,t.code,t.name,"+Language.Select.concatCodeName("sectionScope")+ QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE+ " ORDER BY t.code ASC";
	Collection<Scope> readWhereTypeIsUSBAndFilter(QueryExecutorArguments arguments);
	
	/* count where type is usb and filter */
	String QUERY_NAME_COUNT_WHERE_TYPE_IS_USB_AND_FILTER = "countWhereTypeIsUSBAndFilter";
	String QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_USB_AND_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_TYPE_IS_USB_AND_FILTER);
	String QUERY_VALUE_COUNT_WHERE_TYPE_IS_USB_AND_FILTER = "SELECT COUNT(t.identifier) "+QUERY_VALUE_READ_WHERE_TYPE_IS_USB_AND_FILTER_FROM_WHERE;
	Long countWhereTypeIsUSBAndFilter(QueryExecutorArguments arguments);
	
	/**************************************************************************************************************************************************************************/
	
	/* read all order by type code asscending by code ascending */
	String QUERY_NAME_READ_ALL_01 = "read.all.01";
	String QUERY_IDENTIFIER_READ_ALL_01 = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_ALL_01);
	String QUERY_VALUE_READ_ALL_01 = Language.of(Language.Select.of("t"),"From Scope t"
			,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readAll01();
	
	/* count all */
	String QUERY_NAME_COUNT_ALL_01 = "count.all.01";
	String QUERY_IDENTIFIER_COUNT_ALL_01 = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_ALL_01);
	String QUERY_VALUE_COUNT_ALL_01 = Language.of(Language.Select.of("COUNT(t.identifier)"),"From Scope t");
	Long countAll01();
	
	/* read by codes by types codes order by code ascending */
	String QUERY_NAME_READ_BY_CODES_BY_TYPES_CODES = "readByCodesByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.code IN :"+PARAMETER_NAME_CODES,"t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Scope> readByCodesByTypesCodes(Collection<String> codes,Collection<String> typesCodes);
	
	/* read by actors codes by scopes types codes order by type code by code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES_BY_TYPES_CODES = "readByActorsCodesByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_ACTORS_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES_FROM_WHERE = Language.of(Language.From.of("Scope t JOIN ActorScope actorScope ON actorScope.scope = t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,"actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)));
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("t"),QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES_FROM_WHERE
			,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* count by actors codes by scopes types codes */
	String QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = "countByActorsCodesByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t)"),QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES_FROM_WHERE);
	Long countByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* read by actors codes not associated by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, "readByActorsCodesNotAssociatedByTypesCodes");
	String QUERY_VALUE_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
					"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES+")"))			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Scope> readByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* count by actors codes not associated by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
					"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code IN :"
							+PARAMETER_NAME_ACTORS_CODES+")"))
			);
	Long countByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* read by actor code not associated by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, "readByActorCodeNotAssociatedByTypesCodes");
	String QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES_FROM_WHERE = Language.of(Language.From.of("Scope t")
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
			"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+")")));
	String QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES_FROM_WHERE,Language.Order.of("t.code ASC"));
	
	/* count by actors codes not associated by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES_FROM_WHERE);
	
	/* read where filter not associated order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().build(Scope.class, "readWhereFilterNotAssociated");
	String QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED_FROM_WHERE = Language.of(Language.From.of("Scope t")
			,Language.Where.of(Language.Where.and(
			"t.type.code = :"+PARAMETER_NAME_TYPE_CODE
			,Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+")")
		));
	String QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED = Language.of(Language.Select.of("t")
			,QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED_FROM_WHERE,Language.Order.of("t.code ASC"));
	Collection<Scope> readWhereFilterNotAssociated(QueryExecutorArguments arguments);
	
	/* count by actors codes not associated by types codes */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED);
	String QUERY_VALUE_COUNT_WHERE_FILTER_NOT_ASSOCIATED = Language.of(Language.Select.of("COUNT(t.identifier)")
			,QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED_FROM_WHERE);
	Long countWhereFilterNotAssociated(QueryExecutorArguments arguments);
	
	/* read by types codes order by code ascending */
	String QUERY_NAME_READ_BY_TYPES_CODES = "readByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))			
			,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readByTypesCodes(Collection<String> typesCodes);
	
	/* count by types codes */
	String QUERY_NAME_COUNT_BY_TYPES_CODES = "countByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
			);
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read where function does not exist by types identifiers order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_FUNCTION_DOES_NOT_EXIST_BY_TYPES_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(Scope.class, "readWhereFunctionDoesNotExistByTypesIdentifiers");
	Collection<Scope> readWhereFunctionDoesNotExistByTypesIdentifiers(Collection<String> typesIdentifiers);
	
	/* read where function does not exist by functions identifier by types identifiers order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_FUNCTION_DOES_NOT_EXIST_BY_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(Scope.class, "readWhereFunctionDoesNotExistByTypesIdentifiersByFunctionsIdentifiers");
	Collection<Scope> readWhereFunctionDoesNotExistByTypesIdentifiersByFunctionsIdentifiers(Collection<String> typesIdentifiers,Collection<String> functionsIdentifiers);
	
	/* read where type is ua order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_UA = "readWhereTypeIsUA";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_TYPE_IS_UA);
	Map<String,Integer> QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE
			,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA_FROM = " FROM Scope t "
			+ " JOIN AdministrativeUnit administrativeUnit ON administrativeUnit.identifier = t.identifier "
			+ " JOIN Section section ON section.identifier = administrativeUnit.section "
			+ " JOIN Scope sectionScope ON sectionScope.identifier = section.identifier ";
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA = "SELECT t.identifier,t.code,t.name,"+Language.Select.concatCodeName("sectionScope")+ QUERY_VALUE_READ_WHERE_TYPE_IS_UA_FROM+ " ORDER BY t.code ASC";
	
	/* count where type is ua */
	String QUERY_NAME_COUNT_WHERE_TYPE_IS_UA = "countWhereTypeIsUA";
	String QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_TYPE_IS_UA);
	String QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA = "SELECT COUNT(t.identifier) "+QUERY_VALUE_READ_WHERE_TYPE_IS_UA_FROM;
	
	Collection<Scope> readVisibleByActorCode(String actorCode);
	Long countVisibleByActorCode(String actorCode);
	
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Scope.class
			, "readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier");
	Collection<Scope> readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(QueryExecutorArguments arguments);
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER = QueryIdentifierBuilder.getInstance()
			.buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER);
	Long countWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Scope.class
			, "readWhereCodeOrNameLikeByTypeIdentifier");
	Collection<Scope> readWhereCodeOrNameLikeByTypeIdentifier(QueryExecutorArguments arguments);
	String QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER = QueryIdentifierBuilder.getInstance()
			.buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER);
	Long countWhereCodeOrNameLikeByTypeIdentifier(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Scope> implements ScopeQuerier,Serializable {
		
		@Override
		protected Class<Scope> getKlass() {
			return Scope.class;
		}
		
		@Override
		public Scope readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(Scope.class,arguments.setQuery(null));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<Scope> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(Scope.class,arguments.setQuery(null));
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereTypeIsUAAndFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereTypeIsUSBAndFilter(arguments);			
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterNotAssociated(arguments);			
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(arguments);			
			if(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readWhereCodeOrNameLikeByTypeIdentifier(arguments);
			/*if(ScopeQuerier.QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByCodesByTypesCodes((Collection<String>)arguments.getFilterFieldValue(PARAMETER_NAME_CODES)
						, (Collection<String>)arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));
			*/
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(Scope.class,arguments.setQuery(null));
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereTypeIsUAAndFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_USB_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereTypeIsUSBAndFilter(arguments);			
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilterNotAssociated(arguments);			
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return countWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(arguments);		
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return countWhereCodeOrNameLikeByTypeIdentifier(arguments);
			return super.count(arguments);
		}
		
		@Override
		public Collection<Scope> readWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_TYPE_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_TYPE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_TYPE_NAME, arguments);	
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContains(PARAMETER_NAME_ACTOR_CODE, arguments);
			filter.addFieldsNullable(arguments, PARAMETER_NAME_ACTOR_CODE);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readWhereTypeIsUAAndFilter(QueryExecutorArguments arguments) {
			prepareWhereTypeIsUAAndFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereTypeIsUAAndFilter(QueryExecutorArguments arguments) {
			prepareWhereTypeIsUAAndFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereTypeIsUAAndFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readWhereTypeIsUSBAndFilter(QueryExecutorArguments arguments) {
			prepareWhereTypeIsUSBAndFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereTypeIsUSBAndFilter(QueryExecutorArguments arguments) {
			prepareWhereTypeIsUSBAndFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereTypeIsUSBAndFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readWhereFilterNotAssociated(QueryExecutorArguments arguments) {
			prepareWhereFilterNotAssociated(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereFilterNotAssociated(QueryExecutorArguments arguments) {
			prepareWhereFilterNotAssociated(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilterNotAssociated(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsEquals(arguments,PARAMETER_NAME_TYPE_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readAll01() {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_ALL_01);
		}
		
		@Override
		public Long countAll01() {
			return EntityCounter.getInstance().count(Scope.class,QUERY_IDENTIFIER_COUNT_ALL_01);
		}
		
		@Override
		public Collection<Scope> readByCodesByTypesCodes(Collection<String> codes,Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES
					, PARAMETER_NAME_CODES,codes,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Collection<Scope> readByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES
					, PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Scope.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES)
					.addFilterFieldsValues(PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes));
		}
		
		@Override
		public Collection<Scope> readByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
					, PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Scope.class, new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES).addFilterFieldsValues(
					PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes));
		}
		
		@Override
		public Collection<Scope> readByTypesCodes(Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Scope.class, new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES).addFilterFieldsValues(PARAMETER_NAME_TYPES_CODES,typesCodes));
		}
		
		@Override
		public Collection<Scope> readWhereFunctionDoesNotExistByTypesIdentifiers(Collection<String> typesIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FUNCTION_DOES_NOT_EXIST_BY_TYPES_IDENTIFIERS
					, PARAMETER_NAME_TYPES_IDENTIFIERS,typesIdentifiers);
		}
		
		@Override
		public Collection<Scope> readWhereFunctionDoesNotExistByTypesIdentifiersByFunctionsIdentifiers(Collection<String> typesIdentifiers,Collection<String> functionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FUNCTION_DOES_NOT_EXIST_BY_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS
					, PARAMETER_NAME_FUNCTIONS_IDENTIFIERS,functionsIdentifiers, PARAMETER_NAME_TYPES_IDENTIFIERS,typesIdentifiers);
		}
		
		@Override
		public Collection<Scope> readVisibleByActorCode(String actorCode) {
			Collection<Scope> scopes = null;
			Collection<Scope> sections = ScopeOfTypeSectionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
					.addFilterField(PARAMETER_NAME_ACTOR_CODE, actorCode));
			if(CollectionHelper.isNotEmpty(sections)) {
				if(scopes == null)
					scopes = new ArrayList<>();
				scopes.addAll(sections);
			}
			Collection<Scope> administrativeUnits = ScopeOfTypeAdministrativeUnitQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
					.addFilterField(PARAMETER_NAME_ACTOR_CODE, actorCode));
			if(CollectionHelper.isNotEmpty(administrativeUnits)) {
				if(scopes == null)
					scopes = new ArrayList<>();
				scopes.addAll(administrativeUnits);
			}
			return scopes;
		}
		
		@Override
		public Long countVisibleByActorCode(String actorCode) {
			return NumberHelper.getLong(NumberHelper.add(ScopeOfTypeSectionQuerier.getInstance().countVisibleWhereFilter(
					new QueryExecutorArguments().addFilterField(ScopeOfTypeSectionQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode))
					,ScopeOfTypeAdministrativeUnitQuerier.getInstance().countVisibleWhereFilter(new QueryExecutorArguments()
							.addFilterField(PARAMETER_NAME_ACTOR_CODE, actorCode))));
		}
	
		@Override
		public Collection<Scope> readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(QueryExecutorArguments arguments) {
			prepareWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(QueryExecutorArguments arguments) {
			prepareWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		@Override
		public Collection<Scope> readWhereCodeOrNameLikeByTypeIdentifier(QueryExecutorArguments arguments) {
			prepareWhereCodeOrNameLikeByTypeIdentifier(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereCodeOrNameLikeByTypeIdentifier(QueryExecutorArguments arguments) {
			prepareWhereCodeOrNameLikeByTypeIdentifier(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldEquals(PARAMETER_NAME_TYPE_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_FUNCTION_IDENTIFIER, arguments);
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		private static void prepareWhereCodeOrNameLikeByTypeIdentifier(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldEquals(PARAMETER_NAME_TYPE_IDENTIFIER, arguments);
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	static ScopeQuerier getInstance() {
		return Helper.getInstance(ScopeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static String getQueryValueReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifierWhere() {
		return where(and(
				parenthesis(or(
						like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
						,like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					))
				,"t.type.identifier = :"+PARAMETER_NAME_TYPE_IDENTIFIER
				,not(exists("SELECT sf FROM ScopeFunction sf WHERE sf.scope = t AND sf.function.identifier = :"+PARAMETER_NAME_FUNCTION_IDENTIFIER))
			));
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier() {
		return jpql(
				select(				
					Select.fields("t",Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME)
				)
				,jpql(
					From.ofTuple(Scope.class)
				)
				,getQueryValueReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifierWhere()
				,order(Order.join(asc("t", Scope.FIELD_CODE)))
			);
	}
	
	static String getQueryCountReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier() {
		return jpql(select("COUNT(t.identifier)"),From.ofTuple(Scope.class),getQueryValueReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifierWhere());
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByTypeIdentifierWhere() {
		return where(and(
				parenthesis(or(
						like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
						,like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					))
				,"t.type.identifier = :"+PARAMETER_NAME_TYPE_IDENTIFIER
			));
	}
	
	static String getQueryValueReadWhereCodeOrNameLikeByTypeIdentifier() {
		return jpql(
				select(				
					Select.fields("t",Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME)
				)
				,jpql(
					From.ofTuple(Scope.class)
				)
				,getQueryValueReadWhereCodeOrNameLikeByTypeIdentifierWhere()
				,order(Order.join(asc("t", Scope.FIELD_CODE)))
			);
	}
	
	static String getQueryCountReadWhereCodeOrNameLikeByTypeIdentifier() {
		return jpql(select("COUNT(t.identifier)"),From.ofTuple(Scope.class),getQueryValueReadWhereCodeOrNameLikeByTypeIdentifierWhere());
	}
	
	static void initialize() {
		QueryManager.getInstance().register(
				Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_ALL_01,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_ALL_01)
				
				,Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER
						, getQueryValueReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier())
				.setTupleFieldsNamesIndexesFromFieldsNames(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER
						, getQueryCountReadWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier())
				
				,Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER
						, getQueryValueReadWhereCodeOrNameLikeByTypeIdentifier())
				.setTupleFieldsNamesIndexesFromFieldsNames(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME)
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_CODE_OR_NAME_LIKE_BY_TYPE_IDENTIFIER
						, getQueryCountReadWhereCodeOrNameLikeByTypeIdentifier())
				
				,Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FUNCTION_DOES_NOT_EXIST_BY_TYPES_IDENTIFIERS
						, "SELECT t FROM Scope t WHERE t.type.identifier IN :"+PARAMETER_NAME_TYPES_IDENTIFIERS
						+" AND NOT EXISTS(SELECT sf FROM ScopeFunction sf WHERE sf.scope = t)")
				,Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FUNCTION_DOES_NOT_EXIST_BY_TYPES_IDENTIFIERS_BY_FUNCTIONS_IDENTIFIERS
						, "SELECT t FROM Scope t WHERE t.type.identifier IN :"+PARAMETER_NAME_TYPES_IDENTIFIERS
						+" AND NOT EXISTS(SELECT sf FROM ScopeFunction sf WHERE sf.scope = t AND sf.function.identifier IN :"+PARAMETER_NAME_FUNCTIONS_IDENTIFIERS+") ORDER BY t.code ASC")
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_ALL_01
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class,Query.FIELD_VALUE,QUERY_VALUE_COUNT_ALL_01));
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES,Query.FIELD_TUPLE_CLASS,Scope.class
				,Query.FIELD_RESULT_CLASS,Scope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_CODES_BY_TYPES_CODES
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES,Query.FIELD_TUPLE_CLASS,Scope.class
				,Query.FIELD_RESULT_CLASS,Scope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES
				)
			);
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				)
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED
				)
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER_NOT_ASSOCIATED
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_TYPES_CODES
				)
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_TYPES_CODES
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME))
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_TYPE_IS_UA
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_TUPLE_FIELDS_NAMES_INDEXES)
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA_AND_FILTER
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_TYPE_IS_USB_AND_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_USB_AND_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_TYPE_IS_USB_AND_FILTER
				)
			);
		/*
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_IDENTIFIERS
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,"SELECT s FROM Scope s WHERE s.identifier IN :"+PARAMETER_NAME_IDENTIFIERS+" ORDER BY s.code ASC"));		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_IDENTIFIERS
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,"SELECT COUNT(s.identifier) FROM Scope s WHERE s.identifier IN :"+PARAMETER_NAME_IDENTIFIERS));
		*/
		ScopeOfTypeSectionQuerier.initialize();
		
		ScopeOfTypeBudgetSpecializationUnitQuerier.initialize();
		ScopeOfTypeActionQuerier.initialize();
		ScopeOfTypeActivityCategoryQuerier.initialize();
		ScopeOfTypeActivityQuerier.initialize();
		ScopeOfTypeImputationQuerier.initialize();
		
		ScopeOfTypeAdministrativeUnitQuerier.initialize();
	}

	/**/
	
	static String getPredicateHasBeenMarkedVisible(String parameterNameActorCode) {
		return exists(
				select("v.identifier")
				,from("ActorScope v")
				,where(and("v.actor.code = "+parameterNameActorCode,"v.scope = scope","(v.visible IS NULL OR v.visible = true)"))
			);
	}
	
	static String getPredicateHasBeenMarkedVisible() {
		return getPredicateHasBeenMarkedVisible(":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static String getPredicateHasVisibleChild(String tupleName,String variableName,String fieldName,String parameterNameActorCode) {
		return 
			exists(
				select("actorScope.identifier")
				,from("ActorScope actorScope")
				,"JOIN Scope scopeChild ON actorScope.scope = scopeChild"
				,"JOIN "+tupleName+" "+variableName+" ON "+variableName+" = scopeChild"
				,where(and(variableName+"."+fieldName+" = scope","actorScope.actor.code = "+parameterNameActorCode))
			);
	}
	
	static String getPredicateHasVisibleChild(String tupleName,String variableName,String fieldName) {
		return getPredicateHasVisibleChild(tupleName, variableName, fieldName, ":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static String getPredicateHasVisibleChild(Class<?> klass,String fieldName,String parameterNameActorCode) {
		return getPredicateHasVisibleChild(klass.getSimpleName(), StringHelper.getVariableNameFrom(klass.getSimpleName()), fieldName,parameterNameActorCode);
	}
	
	static String getPredicateHasVisibleChild(Class<?> klass,String fieldName) {
		return getPredicateHasVisibleChild(klass, fieldName,":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static String getPredicateHasVisibleParent(String parentTupleName,String parentVariableName,String tupleName,String variableName,String fieldName,String parameterNameActorCode) {
		return 
			exists(and(
				jpql(
						select("actorScope.identifier")
						,from("ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent")
						,"JOIN "+parentTupleName+" "+parentVariableName+" ON "+parentVariableName+" = scopeParent"
						,where("actorScope.actor.code = "+parameterNameActorCode)
				)
				,exists(
						select(variableName)
						,from(tupleName+" "+variableName)
						,where(and(
								variableName+" = scope",variableName+"."+fieldName+" = "+fieldName
								,not(
										exists(select("actorScope"+tupleName+" ")+from("ActorScope actorScope"+tupleName+" ")
											+ where(and("actorScope"+tupleName+".scope = scope"
													,"actorScope"+tupleName+".actor.code = "+parameterNameActorCode
													,"actorScope"+tupleName+".visible IS NOT NULL","actorScope"+tupleName+".visible = false"
													))
											)
									)
						))
				)
			));
	}
	
	static String getPredicateHasVisibleParent(String parentTupleName,String tupleName,String parameterNameActorCode) {
		String variableName = StringHelper.getVariableNameFrom(parentTupleName);
		return getPredicateHasVisibleParent(parentTupleName, variableName, tupleName, StringHelper.getVariableNameFrom(tupleName), variableName,parameterNameActorCode);
	}
	
	static String getPredicateHasVisibleParent(String parentTupleName,String tupleName) {
		return getPredicateHasVisibleParent(parentTupleName, tupleName, ":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static void addParentCodeNameContains(QueryExecutorArguments arguments,Filter filter,Collection<Class<?>> classes) {
		for(Class<?> klass : classes) {
			String variableName = StringHelper.getVariableNameFrom(klass.getSimpleName()+"CodeName");
			filter.addFieldsContains(arguments,variableName);
			filter.addFieldContainsStringOrWords(variableName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);	
		}
	}
	
	static void addParentCodeNameContains(QueryExecutorArguments arguments,Filter filter,Class<?>...classes) {
		addParentCodeNameContains(arguments, filter, CollectionHelper.listOf(classes));
	}
	
	static String getFromWhere(Class<?> klass,String variableName,String whereClause) {
		return jpql(String.format("FROM Scope scope JOIN %1$s %2$s ON %2$s = scope",klass.getSimpleName(),variableName),whereClause);
	}
}