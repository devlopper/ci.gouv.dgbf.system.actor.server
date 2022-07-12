package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Where.or;

import java.io.Serializable;

import org.apache.commons.lang3.ArrayUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Language.Where;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetCategory;

public interface ScopeOfTypeBudgetCategoryQuerier extends ScopeOfTypeQuerier {

	Class<?> CLASS = BudgetCategory.class;
	String SCOPE_TYPE = ScopeType.CODE_SECTION;
	
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
	
	public static abstract class AbstractImpl extends ScopeOfTypeQuerier.AbstractImpl implements ScopeOfTypeBudgetCategoryQuerier,Serializable {
		
		@Override
		public String buildQueryValueReadWhereFilterFromWhere() {
			return jpql(from("BudgetCategory t")
					,Where.of(Where.and(				
							Where.like("t", BudgetCategory.FIELD_CODE, PARAMETER_NAME_CODE)
							,Where.like("t", BudgetCategory.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
						))
					);
		}
		
		@Override
		public String buildQueryValueReadVisibleWhereFilterWherePredicateVisible(String parameterNameActorCode) {
			return parenthesis(or(
					ScopeQuerier.getPredicateHasBeenMarkedVisible(parameterNameActorCode)
					,getPredicateHasVisibleChild(AdministrativeUnit.class,parameterNameActorCode)
					,getPredicateHasVisibleChild(BudgetSpecializationUnit.class,parameterNameActorCode)
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
					);
		}
		
		@Override
		public String buildQueryValueReadInvisibleWhereFilterWhere() {
			return "WHERE "+Language.Where.and("scope.type.code = '"+SCOPE_TYPE+"'"
					,"NOT "+buildQueryValueReadVisibleWhereFilterWherePredicateVisible(),Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
					,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
		}
	}
	
	/**/
	
	static ScopeOfTypeBudgetCategoryQuerier getInstance() {
		return Helper.getInstance(ScopeOfTypeBudgetCategoryQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		ScopeOfTypeQuerier.initialize(getInstance());
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
		return getPredicateHasVisibleChild(klass, Language.formatParameter(PARAMETER_NAME_ACTOR_CODE));
	}
}