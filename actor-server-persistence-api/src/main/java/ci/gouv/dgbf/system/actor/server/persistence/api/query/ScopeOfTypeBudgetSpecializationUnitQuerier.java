package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.fields;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.or;

import java.io.Serializable;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Language.Select;
import org.cyk.utility.persistence.query.Language.Where;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface ScopeOfTypeBudgetSpecializationUnitQuerier extends ScopeOfTypeQuerier {

	Class<?> CLASS = BudgetSpecializationUnit.class;
	String SCOPE_TYPE = ScopeType.CODE_USB;
	
	String PARAMETER_NAME_SECTION_CODE = "sectionCode";
	String PARAMETER_NAME_SECTION_NAME = "sectionName";
	
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
	public static abstract class AbstractImpl extends ScopeOfTypeQuerier.AbstractImpl implements ScopeOfTypeBudgetSpecializationUnitQuerier,Serializable {
	
		@Override
		public void prepareVisibleWhereFilterAddFieldsCodeAndName(QueryExecutorArguments arguments, Filter filter,String parameterNameCode, String parameterNameName) {
			super.prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter, parameterNameCode, parameterNameName);
			ScopeQuerier.addParentCodeNameContains(arguments, filter, Section.class);
		}
		
		/*protected void prepareVisibleWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			
			ScopeQuerier.addParentCodeNameContains(arguments, filter, Section.class);
			arguments.setFilter(filter);
		}*/
		
		
		protected void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		protected void prepareInvisibleWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleWhereFilter(arguments);
		}
		
		@Override
		public String buildQueryValueReadWhereFilterFromWhere() {
			return jpql(from("BudgetSpecializationUnit t")
					,Where.of(Where.and(				
							Where.like("t", BudgetSpecializationUnit.FIELD_CODE, PARAMETER_NAME_CODE)
							,Where.like("t", BudgetSpecializationUnit.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
							,Where.like("t", BudgetSpecializationUnit.FIELD_SECTION_CODE_NAME, ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
						))
					);
		}
		
		@Override
		public String buildQueryValueReadVisibleWhereFilterWherePredicateVisible(String parameterNameActorCode) {
			return parenthesis(or(
					ScopeQuerier.getPredicateHasBeenMarkedVisible(parameterNameActorCode)
					,getPredicateHasVisibleParent(Section.class,parameterNameActorCode)
					,getPredicateHasVisibleChild(Activity.class,parameterNameActorCode)
					,getPredicateHasVisibleChild(Imputation.class,parameterNameActorCode)
			));
		}
		
		@Override
		public String buildQueryValueReadVisibleWhereFilterWherePredicate(String parameterNameCode,String parameterNameName,String parameterNameActorCode) {
			return Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
					,buildQueryValueReadVisibleWhereFilterWherePredicateVisible(parameterNameActorCode)
					,Where.like("scope", Scope.FIELD_CODE, parameterNameCode)
					,Where.like("scope", Scope.FIELD_NAME, parameterNameName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,Where.like("b", BudgetSpecializationUnit.FIELD_SECTION_CODE_NAME, ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					);
		}
		
		@Override
		public String buildQueryValueReadVisibleWhereFilterFromWhere() {
			return ScopeQuerier.getFromWhere(BudgetSpecializationUnit.class, "b", buildQueryValueReadVisibleWhereFilterWhere());
		}
		
		@Override
		public String buildQueryValueReadInvisibleWhereFilterWhere() {
			return "WHERE "+Language.Where.and(
					"scope.type.code = '"+SCOPE_TYPE+"'"
					,"NOT "+buildQueryValueReadVisibleWhereFilterPredicateVisible()
					,Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
					,Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,Where.like("b", BudgetSpecializationUnit.FIELD_SECTION_CODE_NAME, ScopeQuerier.PARAMETER_NAME_SECTION_CODE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					);
		}
		
		@Override
		public String buildQueryValueReadInvisibleWhereFilterFromWhere() {
			return ScopeQuerier.getFromWhere(BudgetSpecializationUnit.class, "b",buildQueryValueReadInvisibleWhereFilterWhere());
		}
		
		@Override
		public String buildQueryValueReadVisibleWhereFilterPredicateVisible() {
			return parenthesis(or(
					ScopeQuerier.getPredicateHasBeenMarkedVisible()
					,getPredicateHasVisibleParent(Section.class)
					,getPredicateHasVisibleChild(Activity.class)
					,getPredicateHasVisibleChild(Imputation.class)
			));
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
				,Query.FIELD_VALUE,jpql(select(fields("scope","identifier","code","name"),fields("b","sectionCodeName"))
				,getInstance().buildQueryValueReadVisibleWhereFilterFromWhere(), order(asc("scope","code")))
				).setTupleFieldsNamesIndexesFromFieldsNames(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,jpql(select("COUNT(scope.identifier)") ,getInstance().buildQueryValueReadVisibleWhereFilterFromWhere())
				)
			);
	
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,jpql(select(fields("scope","identifier","code","name"),fields("b","sectionCodeName"))
				,getInstance().buildQueryValueReadInvisibleWhereFilterFromWhere(),order(asc("scope","code")))
				).setTupleFieldsNamesIndexesFromFieldsNames(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,jpql(select("COUNT(scope.identifier)") ,getInstance().buildQueryValueReadInvisibleWhereFilterFromWhere())
				)
			);
		
		QueryHelper.addQueries(
				Query.buildSelect(Scope.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
						, jpql(select(Select.fields("t", BudgetSpecializationUnit.FIELD_IDENTIFIER,BudgetSpecializationUnit.FIELD_CODE,BudgetSpecializationUnit.FIELD_NAME
								,BudgetSpecializationUnit.FIELD_SECTION_CODE_NAME))
								, getInstance().buildQueryValueReadWhereFilterFromWhere()
								,order(asc("t",BudgetSpecializationUnit.FIELD_CODE)))
						,MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING))
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, jpql(select("COUNT(t.identifier)"),getInstance().buildQueryValueReadWhereFilterFromWhere()))
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
		return ScopeQuerier.getPredicateHasVisibleChild(klass, "budgetSpecializationUnit",parameterNameActorCode);
	}
	
	static String getPredicateHasVisibleChild(Class<?> klass) {
		return getPredicateHasVisibleChild(klass, Language.formatParameter(PARAMETER_NAME_ACTOR_CODE));
	}
	
	static String getPredicateHasVisibleParent(Class<?> klass,String parameterNameActorCode) {
		return ScopeQuerier.getPredicateHasVisibleParent(klass.getSimpleName(), "BudgetSpecializationUnit",parameterNameActorCode);
	}
	
	static String getPredicateHasVisibleParent(Class<?> klass) {
		return getPredicateHasVisibleParent(klass, Language.formatParameter(PARAMETER_NAME_ACTOR_CODE));
	}
}