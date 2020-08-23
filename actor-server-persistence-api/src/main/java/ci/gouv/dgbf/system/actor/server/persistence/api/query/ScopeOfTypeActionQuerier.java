package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
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

import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface ScopeOfTypeActionQuerier extends Querier {

	String SCOPE_TYPE = ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType.CODE_ACTION;
	
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/* read where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readActionsWhereFilter");
	Collection<Scope> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleActionsWhereFilter");
	Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER);
	Long countVisibleWhereFilter(QueryExecutorArguments arguments);

	/* read invisible where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleActionsWhereFilter");
	Collection<Scope> readInvisibleWhereFilter(QueryExecutorArguments arguments);
	
	/* count invisible where filter */
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER);
	Long countInvisibleWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeOfTypeActionQuerier,Serializable {
		
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
			filter.addFieldContainsStringOrWords(ScopeQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
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
			
			ScopeQuerier.addParentCodeNameContains(arguments, filter, Section.class,BudgetSpecializationUnit.class);
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
	}
	
	/**/
	
	static ScopeOfTypeActionQuerier getInstance() {
		return Helper.getInstance(ScopeOfTypeActionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,jpql(select(fields("scope","identifier","code","name"),fields("a","sectionCodeName",Action.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME))
				,getQueryValueReadVisibleWhereFilterFromWhere(), order(asc("scope","code")))
				).setTupleFieldsNamesIndexesFromFieldsNames(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING,Scope.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,jpql(select("COUNT(scope.identifier)") ,getQueryValueReadVisibleWhereFilterFromWhere())
				)
			);
	
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,jpql(select(fields("scope","identifier","code","name"),fields("a","sectionCodeName",Action.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME))
				,getQueryValueReadInvisibleWhereFilterFromWhere(),order(asc("scope","code")))
				).setTupleFieldsNamesIndexesFromFieldsNames(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING,Scope.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,jpql(select("COUNT(scope.identifier)") ,getQueryValueReadInvisibleWhereFilterFromWhere())
				)
			);
		
		QueryHelper.addQueries(
				Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
						, jpql(select(Select.fields("t", Action.FIELD_IDENTIFIER,Action.FIELD_CODE,Action.FIELD_NAME
								,Action.FIELD_SECTION_CODE_NAME,Action.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME))
								, getQueryValueReadWhereFilterFromWhere()
								,order(asc("t",Action.FIELD_CODE)))
						,MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING
								,Scope.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING))
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER
						, jpql(select("COUNT(t.identifier)"),getQueryValueReadWhereFilterFromWhere()))
		);
	}
	
	static String getQueryValueReadWhereFilterFromWhere() {
		return jpql(from("Action t ")
			,Language.Where.of(Language.Where.and(				
					Language.Where.like("t", Action.FIELD_CODE, PARAMETER_NAME_CODE)
					,Language.Where.like("t", Action.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,Language.Where.like("t", Action.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, ScopeQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,Language.Where.like("t", Action.FIELD_SECTION_CODE_NAME, ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				))
			);
	}
	
	static String getQueryValueReadVisibleWhereFilterPredicateVisible() {
		return  parenthesis(or(
				ScopeQuerier.getPredicateHasBeenMarkedVisible()
				,getPredicateHasVisibleParent(Section.class)
				,getPredicateHasVisibleParent(BudgetSpecializationUnit.class)
				,getPredicateHasVisibleChild(Activity.class)
				,getPredicateHasVisibleChild(Imputation.class)
		));
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere() {
		return Where.of(Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
				,getQueryValueReadVisibleWhereFilterPredicateVisible()
				,Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				,Where.like("a", Action.FIELD_SECTION_CODE_NAME, ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				,Where.like("a", Action.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, ScopeQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				));
	}
	
	static String getQueryValueReadVisibleWhereFilterFromWhere() {
		return ScopeQuerier.getFromWhere(Action.class, "a", getQueryValueReadVisibleWhereFilterWhere());
	}
	
	static String getQueryValueReadInvisibleWhereFilterWhere() {
		return "WHERE "+Language.Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
				,"NOT "+getQueryValueReadVisibleWhereFilterPredicateVisible()
				,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				,Where.like("a", Action.FIELD_SECTION_CODE_NAME, ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				,Where.like("a", Action.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, ScopeQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				);
	}
	
	static String getQueryValueReadInvisibleWhereFilterFromWhere() {
		return ScopeQuerier.getFromWhere(Action.class, "a",getQueryValueReadInvisibleWhereFilterWhere());
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

	/**/
	
	static String getPredicateHasVisibleChild(Class<?> klass) {
		return ScopeQuerier.getPredicateHasVisibleChild(klass, "action");
	}
	
	static String getPredicateHasVisibleParent(Class<?> klass) {
		return ScopeQuerier.getPredicateHasVisibleParent(klass.getSimpleName(), "Action");
	}
}