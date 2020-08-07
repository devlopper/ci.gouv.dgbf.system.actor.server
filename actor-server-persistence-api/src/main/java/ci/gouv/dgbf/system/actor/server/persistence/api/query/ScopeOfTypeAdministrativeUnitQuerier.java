package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public interface ScopeOfTypeAdministrativeUnitQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/* read visible administrative units by actor code order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleAdministrativeUnitsByActorCode");
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE = 
			"WHERE scope.type.code = '"+ScopeType.CODE_UA+"' AND ("+
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND actorScope.scope.identifier = scope.identifier AND (actorScope.visible IS NULL OR actorScope.visible = true) "+ 
			"    ) " + 
			"    OR " + 
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        JOIN Scope scopeSection ON actorScope.scope = scopeSection " + 
			"        JOIN Section section ON section = scopeSection " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND EXISTS("
					+ "SELECT administrativeUnit "
					+ "FROM AdministrativeUnit administrativeUnit "
					+ "WHERE administrativeUnit = scope AND administrativeUnit.section = section AND NOT EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope actorScopeAdministrativeUnit "
					+ "WHERE actorScopeAdministrativeUnit.scope = scope AND actorScopeAdministrativeUnit.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND (actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))"
					+ ")" + 
			"    )" + 
			")";
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleAdministrativeUnitsByActorCode(String actorCode);
	
	/* count visible administrative units by actor code */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE);
	String QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE;
	Long countVisibleAdministrativeUnitsByActorCode(String actorCode);
	
	/* read visible administrative units where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleAdministrativeUnitsWhereFilter");
	
	static String getQueryValueReadVisibleAdministrativeUnitsWhereFilterPredicateVisible() {
		return "(EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE
				+" AND actorScope.scope.identifier = scope.identifier AND (actorScope.visible IS NULL OR actorScope.visible = true)) " + 
				" OR " + 
				" EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeSection ON actorScope.scope = scopeSection " + 
				" JOIN Section section ON section = scopeSection WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND EXISTS("
				+ "SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit "
				+ "WHERE administrativeUnit = scope AND administrativeUnit.section = section AND NOT EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope actorScopeAdministrativeUnit "
				+ "WHERE actorScopeAdministrativeUnit.scope = scope AND actorScopeAdministrativeUnit.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND (actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))"
				+ ")))";
	}
	
	static String getQueryValueReadVisibleAdministrativeUnitsWhereFilterWhere() {
		return Language.Where.of(Language.Where.and("scope.type.code = '"+ScopeType.CODE_UA+"'"
				,getQueryValueReadVisibleAdministrativeUnitsWhereFilterPredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				));
	}
	
	Collection<Scope> readVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible administrative units where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER);
	Long countVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible administrative units with sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleAdministrativeUnitsWithSectionsWhereFilter");
	Map<String,Integer> QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = 
			MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+ScopeType.CODE_UA+"'"
			,getQueryValueReadVisibleAdministrativeUnitsWhereFilterPredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = "SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
		+ " FROM Scope scope " 
		+ " JOIN AdministrativeUnit administrativeUnitOfScope ON administrativeUnitOfScope.identifier = scope.identifier "
		+ " JOIN Section sectionOfAdministrativeUnit ON sectionOfAdministrativeUnit.identifier = administrativeUnitOfScope.section "
		+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfAdministrativeUnit.identifier "
		+ QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible administrative units with sections where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE;
	Long countVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read invisible administrative units with sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleAdministrativeUnitsWithSectionsWhereFilter");
	String QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+ScopeType.CODE_UA+"'"
			,"NOT "+getQueryValueReadVisibleAdministrativeUnitsWhereFilterPredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = "SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
		+ " FROM Scope scope " 
		+ " JOIN AdministrativeUnit administrativeUnitOfScope ON administrativeUnitOfScope.identifier = scope.identifier "
		+ " JOIN Section sectionOfAdministrativeUnit ON sectionOfAdministrativeUnit.identifier = administrativeUnitOfScope.section "
		+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfAdministrativeUnit.identifier "
		+ QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count invisible administrative units with sections where filter */
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE;
	Long countInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible administrative units by actor code order by code ascending */
	String QUERY_NAME_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = "readVisibleAdministrativeUnitsWithSectionByActorCode";
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE);
	Map<String,Integer> QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE_TUPLE_FIELDS_NAMES_INDEXES = 
			MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = 
			"SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
			+ " FROM Scope scope " 
			+ " JOIN AdministrativeUnit administrativeUnitOfScope ON administrativeUnitOfScope.identifier = scope.identifier "
			+ " JOIN Section sectionOfAdministrativeUnit ON sectionOfAdministrativeUnit.identifier = administrativeUnitOfScope.section "
			+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfAdministrativeUnit.identifier "
			+ QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE
			+ " ORDER BY scope.code ASC";
	
	/* count visible administrative units by actor code */
	String QUERY_NAME_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = "countVisibleAdministrativeUnitsWithSectionByActorCode";
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE);

	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeOfTypeAdministrativeUnitQuerier,Serializable {
		
		@Override
		public Collection<Scope> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			if(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not readable by "+getClass());
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			/*
			if(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE.equals(arguments.getQuery().getIdentifier())) {
				arguments.getQuery().setIdentifier(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE);
				return super.count(tupleClass, arguments);
			}
			*/
			if(QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countVisibleAdministrativeUnitsWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not countable by "+getClass());
		}
		
		@Override
		public Collection<Scope> readVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER);
			prepareVisibleAdministrativeUnitsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER);
			prepareVisibleAdministrativeUnitsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
			prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
			prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWhereFilter(arguments);
		}
		
		@Override
		public Collection<Scope> readInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
			prepareInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
			prepareInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
		}
		
		@Override
		public Collection<Scope> readVisibleAdministrativeUnitsByActorCode(String actorCode) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
		
		@Override
		public Long countVisibleAdministrativeUnitsByActorCode(String actorCode) {
			return EntityCounter.getInstance().count(Scope.class, QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
	}
	
	/**/
	
	static ScopeOfTypeAdministrativeUnitQuerier getInstance() {
		return Helper.getInstance(ScopeOfTypeAdministrativeUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,"SELECT scope FROM Scope scope " + getQueryValueReadVisibleAdministrativeUnitsWhereFilterWhere()+ " ORDER BY scope.code ASC"
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,"SELECT COUNT(scope.identifier) FROM Scope scope " + getQueryValueReadVisibleAdministrativeUnitsWhereFilterWhere()
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE_TUPLE_FIELDS_NAMES_INDEXES)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				)
			);
	}
	
	String[] QUERIES_IDENTIFIERS = {
			QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
			,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
			};
	
	static Boolean isProcessable(QueryExecutorArguments arguments) {
		if(arguments == null || arguments.getQuery() == null)
			return Boolean.FALSE;
		return ArrayUtils.contains(QUERIES_IDENTIFIERS, arguments.getQuery().getIdentifier());
	}
}