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

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface ScopeOfTypeActionQuerier extends ScopeOfTypeQuerier {

	Class<?> CLASS = Action.class;
	String SCOPE_TYPE = ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType.CODE_ACTION;
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = ScopeOfTypeQuerier.buildReadWhereFilter(CLASS);	
	@Override
	default java.lang.String getQueryIdentifierReadWhereFilter() {
		return QUERY_IDENTIFIER_READ_WHERE_FILTER;
	}
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = ScopeOfTypeQuerier.buildCountWhereFilter(CLASS);
	@Override
	default java.lang.String getQueryIdentifierCountWhereFilter() {
		return QUERY_IDENTIFIER_COUNT_WHERE_FILTER;
	}
	
	String QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER = ScopeOfTypeQuerier.buildReadVisibleWhereFilter(CLASS);	
	@Override
	default java.lang.String getQueryIdentifierReadVisibleWhereFilter() {
		return QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER;
	}
	
	String QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER = ScopeOfTypeQuerier.buildCountVisibleWhereFilter(CLASS);
	@Override
	default java.lang.String getQueryIdentifierCountVisibleWhereFilter() {
		return QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER;
	}
	
	String QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER = ScopeOfTypeQuerier.buildReadInvisibleWhereFilter(CLASS);	
	@Override
	default java.lang.String getQueryIdentifierReadInvisibleWhereFilter() {
		return QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER;
	}
	
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER = ScopeOfTypeQuerier.buildCountInvisibleWhereFilter(CLASS);
	@Override
	default String getQueryIdentifierCountInvisibleWhereFilter() {
		return QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER;
	}
	
	/**/
	
	public static abstract class AbstractImpl extends ScopeOfTypeQuerier.AbstractImpl implements ScopeOfTypeActionQuerier,Serializable {
		
		protected void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(ScopeQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		protected void prepareVisibleWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			
			ScopeQuerier.addParentCodeNameContains(arguments, filter, Section.class,BudgetSpecializationUnit.class);
			arguments.setFilter(filter);
		}
		
		protected void prepareInvisibleWhereFilter(QueryExecutorArguments arguments) {
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