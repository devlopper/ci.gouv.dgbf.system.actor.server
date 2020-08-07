package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
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

public interface ScopeOfTypeBudgetSpecializationUnitQuerier extends Querier {

	String SCOPE_TYPE = ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType.CODE_USB;
	
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
		
	/* read visible budget specialisation units where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleBudgetSpecializationUnitsWhereFilter");
	
	static String getQueryValueReadVisibleWhereFilterPredicateVisible() {
		return "(EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE
				+" AND actorScope.scope.identifier = scope.identifier AND (actorScope.visible IS NULL OR actorScope.visible = true)) " + 
				" OR " + 
				" EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeSection ON actorScope.scope = scopeSection " + 
				" JOIN Section section ON section = scopeSection WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND EXISTS("
				+ "SELECT budgetSpecializationUnit FROM BudgetSpecializationUnit budgetSpecializationUnit "
				+ "WHERE budgetSpecializationUnit = scope AND budgetSpecializationUnit.section = section AND NOT EXISTS(SELECT actorScopeBudgetSpecializationUnit FROM ActorScope actorScopeBudgetSpecializationUnit "
				+ "WHERE actorScopeBudgetSpecializationUnit.scope = scope AND actorScopeBudgetSpecializationUnit.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND (actorScopeBudgetSpecializationUnit.visible IS NOT NULL AND actorScopeBudgetSpecializationUnit.visible = false))"
				+ ")))";
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere() {
		return Language.Where.of(Language.Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
				,getQueryValueReadVisibleWhereFilterPredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				));
	}
	
	Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible budget specialisation units where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	Long countVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible budget specialisation units with sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleBudgetSpecializationUnitsWithSectionsWhereFilter");
	Map<String,Integer> QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = 
			MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
			,getQueryValueReadVisibleWhereFilterPredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER = "SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
		+ " FROM Scope scope " 
		+ " JOIN BudgetSpecializationUnit budgetSpecializationUnitOfScope ON budgetSpecializationUnitOfScope.identifier = scope.identifier "
		+ " JOIN Section sectionOfBudgetSpecializationUnit ON sectionOfBudgetSpecializationUnit.identifier = budgetSpecializationUnitOfScope.section "
		+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfBudgetSpecializationUnit.identifier "
		+ QUERY_VALUE_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible budget specialisation units with sections where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_VISIBLE_WITH_SECTIONS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER_WHERE;
	Long countVisibleWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read invisible budget specialisation units with sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleBudgetSpecializationUnitsWithSectionsWhereFilter");
	String QUERY_VALUE_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
			,"NOT "+getQueryValueReadVisibleWhereFilterPredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER = "SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
		+ " FROM Scope scope " 
		+ " JOIN BudgetSpecializationUnit budgetSpecializationUnitOfScope ON budgetSpecializationUnitOfScope.identifier = scope.identifier "
		+ " JOIN Section sectionOfBudgetSpecializationUnit ON sectionOfBudgetSpecializationUnit.identifier = budgetSpecializationUnitOfScope.section "
		+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfBudgetSpecializationUnit.identifier "
		+ QUERY_VALUE_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readInvisibleWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count invisible budget specialisation units with sections where filter */
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER_WHERE;
	Long countInvisibleWithSectionsWhereFilter(QueryExecutorArguments arguments);

	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeOfTypeBudgetSpecializationUnitQuerier,Serializable {
		
		@Override
		public Collection<Scope> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readVisibleWithSectionsWhereFilter(arguments);
			if(QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readInvisibleWithSectionsWhereFilter(arguments);
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not readable by "+getClass());
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countVisibleWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_VISIBLE_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countVisibleWithSectionsWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countInvisibleWithSectionsWhereFilter(arguments);
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not countable by "+getClass());
		}
		
		@Override
		public Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
			prepareVisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER);
			prepareVisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readVisibleWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER);
			prepareVisibleWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_VISIBLE_WITH_SECTIONS_WHERE_FILTER);
			prepareVisibleWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleWhereFilter(arguments);
		}
		
		@Override
		public Collection<Scope> readInvisibleWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER);
			prepareInvisibleWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countInvisibleWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER);
			prepareInvisibleWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareInvisibleWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleWithSectionsWhereFilter(arguments);
		}
	}
	
	/**/
	
	static ScopeOfTypeBudgetSpecializationUnitQuerier getInstance() {
		return Helper.getInstance(ScopeOfTypeBudgetSpecializationUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,"SELECT scope FROM Scope scope " + getQueryValueReadVisibleWhereFilterWhere()+ " ORDER BY scope.code ASC"
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,"SELECT COUNT(scope.identifier) FROM Scope scope " + getQueryValueReadVisibleWhereFilterWhere()
				)
			);
	
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_WITH_SECTIONS_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER
				)
			);
	}
	
	String[] QUERIES_IDENTIFIERS = {
			QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_VISIBLE_WITH_SECTIONS_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_INVISIBLE_WITH_SECTIONS_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WITH_SECTIONS_WHERE_FILTER
			};
	
	static Boolean isProcessable(QueryExecutorArguments arguments) {
		if(arguments == null || arguments.getQuery() == null)
			return Boolean.FALSE;
		return ArrayUtils.contains(QUERIES_IDENTIFIERS, arguments.getQuery().getIdentifier());
	}
}