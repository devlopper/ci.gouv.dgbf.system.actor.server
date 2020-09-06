package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Language;
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

import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface ScopeOfTypeSectionQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/* read where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readSectionsWhereFilter");
	Collection<Scope> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleSectionsWhereFilter");
	Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible sections where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	Long countVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* read invisible sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleSectionsWhereFilter");
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
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not readable by "+getClass());
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countVisibleWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countInvisibleWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
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
			prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter);
			arguments.setFilter(filter);
		}
		
		public static void prepareVisibleWhereFilterAddFieldsCodeAndName(QueryExecutorArguments arguments,Filter filter,String parameterNameCode,String parameterNameName) {
			filter.addFieldsContains(arguments,parameterNameCode);
			filter.addFieldContainsStringOrWords(parameterNameName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		}
		
		public static void prepareVisibleWhereFilterAddFieldsCodeAndName(QueryExecutorArguments arguments,Filter filter) {
			prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter, PARAMETER_NAME_CODE, PARAMETER_NAME_NAME);
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
		
		@Override
		public Collection<Scope> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	static ScopeOfTypeSectionQuerier getInstance() {
		return Helper.getInstance(ScopeOfTypeSectionQuerier.class, INSTANCE);
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
		
		QueryHelper.addQueries(
				Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
						, jpql(select(Select.fields("t", Section.FIELD_IDENTIFIER,Section.FIELD_CODE,Section.FIELD_NAME))
								, getQueryValueReadWhereFilterFromWhere()
								,order(asc("t",Section.FIELD_CODE)))
						,MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME))
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, jpql(select("COUNT(t.identifier)"),getQueryValueReadWhereFilterFromWhere()))
		);
	}
	
	static String getQueryValueReadVisibleWhereFilterWherePredicateVisible(String parameterNameActorCode) {
		return parenthesis(or(
				ScopeQuerier.getPredicateHasBeenMarkedVisible(parameterNameActorCode)
				,getPredicateHasVisibleChild(AdministrativeUnit.class,parameterNameActorCode)
				,getPredicateHasVisibleChild(BudgetSpecializationUnit.class,parameterNameActorCode)
				,getPredicateHasVisibleChild(Activity.class,parameterNameActorCode)
				,getPredicateHasVisibleChild(Imputation.class,parameterNameActorCode)
			));
	}
	
	static String getQueryValueReadVisibleWhereFilterWherePredicateVisible() {
		return getQueryValueReadVisibleWhereFilterWherePredicateVisible(":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static String getQueryValueReadVisibleWhereFilterWherePredicate(String parameterNameCode,String parameterNameName,String parameterNameActorCode) {
		return Where.and("scope.type.code = '"+ScopeType.CODE_SECTION+"'"
				,getQueryValueReadVisibleWhereFilterWherePredicateVisible(parameterNameActorCode)
				,Where.like("scope", Scope.FIELD_CODE, parameterNameCode)
				,Where.like("scope", Scope.FIELD_NAME, parameterNameName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				);
	}
	
	static String getQueryValueReadVisibleWhereFilterWherePredicate(String parameterNameCode,String parameterNameName) {
		return getQueryValueReadVisibleWhereFilterWherePredicate(parameterNameCode, parameterNameName, ":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static String getQueryValueReadVisibleWhereFilterWherePredicate() {
		return getQueryValueReadVisibleWhereFilterWherePredicate(PARAMETER_NAME_CODE, PARAMETER_NAME_NAME);
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere(String parameterNameCode,String parameterNameName,String parameterNameActorCode) {
		return Where.of(getQueryValueReadVisibleWhereFilterWherePredicate(parameterNameCode,parameterNameName,parameterNameActorCode));
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere(String parameterNameCode,String parameterNameName) {
		return getQueryValueReadVisibleWhereFilterWhere(parameterNameCode, parameterNameName, ":"+PARAMETER_NAME_ACTOR_CODE);
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere() {
		return getQueryValueReadVisibleWhereFilterWhere(PARAMETER_NAME_CODE, PARAMETER_NAME_NAME);
	}
	
	static String getQueryValueReadInvisibleWhereFilterWhere() {
		return "WHERE "+Language.Where.and("scope.type.code = '"+ScopeType.CODE_SECTION+"'"
				,"NOT "+getQueryValueReadVisibleWhereFilterWherePredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
	}
	
	static String getQueryValueReadWhereFilterFromWhere() {
		return jpql(from("Section t")
			,Where.of(Where.and(				
					Where.like("t", Section.FIELD_CODE, PARAMETER_NAME_CODE)
					,Where.like("t", Section.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				))
			);
	}
	
	String[] QUERIES_IDENTIFIERS = {
			QUERY_IDENTIFIER_READ_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER
			,QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER
			};
	
	static Boolean isProcessable(QueryExecutorArguments arguments) {
		if(arguments == null || arguments.getQuery() == null)
			return Boolean.FALSE;
		return ArrayUtils.contains(QUERIES_IDENTIFIERS, arguments.getQuery().getIdentifier());
	}
	
	static String getPredicateHasVisibleChild(Class<?> klass,String parameterNameActorCode) {
		return ScopeQuerier.getPredicateHasVisibleChild(klass, "section",parameterNameActorCode);
	}
	
	static String getPredicateHasVisibleChild(Class<?> klass) {
		return getPredicateHasVisibleChild(klass, ":"+PARAMETER_NAME_ACTOR_CODE);
	}
}