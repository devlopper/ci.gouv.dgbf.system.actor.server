package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
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
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface ScopeOfTypeSectionQuerier extends ScopeOfTypeQuerier {

	Class<?> CLASS = Section.class;
	
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
	
	public static abstract class AbstractImpl extends ScopeOfTypeQuerier.AbstractImpl implements ScopeOfTypeSectionQuerier,Serializable {
		
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