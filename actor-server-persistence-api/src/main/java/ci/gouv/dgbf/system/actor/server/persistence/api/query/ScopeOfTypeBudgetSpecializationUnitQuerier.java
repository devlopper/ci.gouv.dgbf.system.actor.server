package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.exists;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.not;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

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
	
	String PARAMETER_NAME_SECTION_CODE = "sectionCode";
	String PARAMETER_NAME_SECTION_NAME = "sectionName";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
		
	/* read visible budget specialisation units where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleBudgetSpecializationUnitsWhereFilter");
	
	static String getQueryValueReadVisibleWhereFilterPredicateVisible() {
		return  parenthesis(or(
				/*Marked Explicitly as visible = true : Read from Actor Scope*/
				exists(
					select("actorScope.identifier"),from("ActorScope actorScope"),where(and(
							"actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE
							,"actorScope.scope.identifier = scope.identifier"
							,"(actorScope.visible IS NULL OR actorScope.visible = true)"
					))
				)
				/*Not Marked Explicitly -> visible = false or visible = null
				 * 1 - Read From Section : Its section is in actor scope
				 * */
				,exists(and(
					jpql(select("actorScope.identifier"),from("ActorScope actorScope JOIN Scope scopeSection ON actorScope.scope = scopeSection")
					,"JOIN Section section ON section = scopeSection",where("actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE))
					,exists(
						select("budgetSpecializationUnit"),from("BudgetSpecializationUnit budgetSpecializationUnit")
						,where(and("budgetSpecializationUnit = scope","budgetSpecializationUnit.section = section",
							not(exists(select("actorScopeBudgetSpecializationUnit ")+from("ActorScope actorScopeBudgetSpecializationUnit ")
							+ where(and("actorScopeBudgetSpecializationUnit.scope = scope","actorScopeBudgetSpecializationUnit.actor.code = :"+PARAMETER_NAME_ACTOR_CODE
								,"actorScopeBudgetSpecializationUnit.visible IS NOT NULL","actorScopeBudgetSpecializationUnit.visible = false")
							)))))
					)
				))
				//From Activity
				,exists(select("actorScope.identifier"),from("ActorScope actorScope")
					,"JOIN Scope scopeActivity ON actorScope.scope = scopeActivity"
					,"JOIN Activity activity ON activity = scopeActivity "
					,where(and("actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE,"activity.budgetSpecializationUnit = scope")))
				//From Imputation
				,exists(select("actorScope.identifier"),from("ActorScope actorScope")
					,"JOIN Scope scopeImputation ON actorScope.scope = scopeImputation"
					,"JOIN ActivityEconomicNature imputation ON imputation = scopeImputation "
					,where(and("actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE,"imputation.budgetSpecializationUnit = scope")))
		));
	}
	
	static String getQueryValueReadVisibleWhereFilterWhere() {
		return Language.Where.of(Language.Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
				,getQueryValueReadVisibleWhereFilterPredicateVisible()
				,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
				,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				/*,parenthesis(or(
						Language.Where.like("section", BudgetSpecializationUnit.FIELD_CODE, PARAMETER_NAME_SECTION_CODE)
						,Language.Where.like("section", BudgetSpecializationUnit.FIELD_NAME, PARAMETER_NAME_SECTION_NAME,NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
						))*/
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
			if(QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readVisibleWhereFilter(arguments);
			
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
			/*filter.addFieldsContains(arguments,PARAMETER_NAME_SECTION_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_SECTION_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			*/
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