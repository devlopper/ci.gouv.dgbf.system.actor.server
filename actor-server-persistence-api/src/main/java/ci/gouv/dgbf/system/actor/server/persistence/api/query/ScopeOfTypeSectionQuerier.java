package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Language;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.exists;

import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
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

public interface ScopeOfTypeSectionQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/* read visible sections by actor code order by code ascending */
	/*
	String QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleSectionsByActorCode");
	String QUERY_VALUE_READ_VISIBLE_BY_ACTOR_CODE_WHERE = 
			"WHERE scope.type.code = '"+ScopeType.CODE_SECTION+"' AND ("+
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND actorScope.scope.identifier = scope.identifier "+ 
			"    ) " + 
			"    OR " + 
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        JOIN Scope scopeUa ON actorScope.scope = scopeUa " + 
			"        JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeUa " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND administrativeUnit.section = scope" + 
			"    )" + 
			")";
	String QUERY_VALUE_READ_VISIBLE_BY_ACTOR_CODE = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_BY_ACTOR_CODE_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleSectionsByActorCode(String actorCode);
	*/
	/* count visible sections by actor code */
	/*
	String QUERY_IDENTIFIER_COUNT_VISIBLE_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE);
	String QUERY_VALUE_COUNT_VISIBLE_BY_ACTOR_CODE = "SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_BY_ACTOR_CODE_WHERE;
	Long countVisibleSectionsByActorCode(String actorCode);
	*/
	/* read visible sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleSectionsWhereFilter");
	static String getQueryValueReadVisibleWhereFilterWherePredicateVisible() {
		return or(
				//From Actor Scope
				exists(Select.of("v.identifier"),From.of("ActorScope v"),Where.of(and("v.actor.code = :"+PARAMETER_NAME_ACTOR_CODE,"v.scope = scope")))
				//From Administrative Unit
				,exists(Select.of("actorScope.identifier"),From.of("ActorScope actorScope")
				,"JOIN Scope scopeUa ON actorScope.scope = scopeUa"
				,"JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeUa"
				,Where.of(and("actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE,"administrativeUnit.section = scope")))
				//From Budget Specialization Unit
				,exists(Select.of("actorScope.identifier"),From.of("ActorScope actorScope")
				,"JOIN Scope scopeBudgetSpecializationUnit ON actorScope.scope = scopeBudgetSpecializationUnit"
				,"JOIN BudgetSpecializationUnit budgetSpecializationUnit ON budgetSpecializationUnit = scopeBudgetSpecializationUnit "
				,Where.of(and("actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE,"budgetSpecializationUnit.section = scope")))
			);
	}
	
	static String getQueryValueReadVisibleWhereFilterWherePredicate() {
		return Where.and("scope.type.code = '"+ScopeType.CODE_SECTION+"'"
				,getQueryValueReadVisibleWhereFilterWherePredicateVisible(),Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere() {
		return Where.of(getQueryValueReadVisibleWhereFilterWherePredicate());
	}
	
	Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible sections where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	Long countVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* read invisible sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleSectionsWhereFilter");
	
	static String getQueryValueReadInvisibleWhereFilterWhere() {
		return "WHERE "+Language.Where.and("scope.type.code = '"+ScopeType.CODE_SECTION+"'"
				,"NOT "+getQueryValueReadVisibleWhereFilterWherePredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
	}
	
	Collection<Scope> readInvisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* count invisible sections where filter */
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER);
	Long countInvisibleWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeOfTypeSectionQuerier,Serializable {
		
		@Override
		public Collection<Scope> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readVisibleWhereFilter(arguments);
			if(QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readInvisibleWhereFilter(arguments);			
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not readable by "+getClass());
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countVisibleWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countInvisibleWhereFilter(arguments);			
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
		public Collection<Scope> readInvisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER);
			prepareInvisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countInvisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER);
			prepareInvisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareInvisibleWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleWhereFilter(arguments);
		}
		
		/*
		@Override
		public Collection<Scope> readVisibleByActorCode(String actorCode) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
		
		@Override
		public Long countVisibleByActorCode(String actorCode) {
			return EntityCounter.getInstance().count(Scope.class, QUERY_IDENTIFIER_COUNT_VISIBLE_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
		*/
	}
	
	/**/
	
	static ScopeOfTypeSectionQuerier getInstance() {
		return Helper.getInstance(ScopeOfTypeSectionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		/*
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_BY_ACTOR_CODE
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_BY_ACTOR_CODE
				)
			);
		*/
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,"SELECT scope FROM Scope scope " + getQueryValueReadInvisibleWhereFilterWhere()+ " ORDER BY scope.code ASC"
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,"SELECT COUNT(scope.identifier) FROM Scope scope " + getQueryValueReadInvisibleWhereFilterWhere()
				)
			);
	}
	
	String[] QUERIES_IDENTIFIERS = {
			/*QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE,QUERY_IDENTIFIER_COUNT_VISIBLE_BY_ACTOR_CODE
			,*/QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER
			};
	
	static Boolean isProcessable(QueryExecutorArguments arguments) {
		if(arguments == null || arguments.getQuery() == null)
			return Boolean.FALSE;
		return ArrayUtils.contains(QUERIES_IDENTIFIERS, arguments.getQuery().getIdentifier());
	}
}