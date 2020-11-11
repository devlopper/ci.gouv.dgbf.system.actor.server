package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.like;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface AssignmentsQuerier extends Querier {

	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	String PARAMETER_NAME_SECTION = "section";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnitCodeName";
	String PARAMETER_NAME_ACTION = "action";
	String PARAMETER_NAME_ACTIVITY = "activity";
	String PARAMETER_NAME_ECONOMIC_NATURE = "economicNature";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT = "administrativeUnit";
	String PARAMETER_NAME_ACTIVITY_CATEGORY = "activityCategory";
	String PARAMETER_NAME_EXPENDITURE_NATURE = "expenditureNature";
	
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER = "creditManagerHolder";
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_NULLABLE = PARAMETER_NAME_CREDIT_MANAGER_HOLDER+"Nullable";
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT = "creditManagerAssistant";
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT_NULLABLE = PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT+"Nullable";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER = "authorizingOfficerHolder";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_NULLABLE = PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER+"Nullable";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT = "authorizingOfficerAssistant";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT_NULLABLE = PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT+"Nullable";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER = "financialControllerHolder";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_NULLABLE = PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER+"Nullable";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT = "financialControllerAssistant";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT_NULLABLE = PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT+"Nullable";
	String PARAMETER_NAME_ACCOUNTING_HOLDER = "accountingHolder";
	String PARAMETER_NAME_ACCOUNTING_HOLDER_NULLABLE = PARAMETER_NAME_ACCOUNTING_HOLDER+"Nullable";
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT = "accountingAssistant";
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT_NULLABLE = PARAMETER_NAME_ACCOUNTING_ASSISTANT+"Nullable";
	
	Assignments readOne(QueryExecutorArguments arguments);
	Collection<Assignments> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(Assignments.class,"readByIdentifierForEdit");
	Assignments readByIdentifierForEdit(String identifier);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Assignments.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<Assignments> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readWhereFilterForApplyModel");
	Collection<Assignments> readWhereFilterForApplyModel(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readWhereFilterForUI");
	Collection<Assignments> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements AssignmentsQuerier,Serializable {		
		
		@Override
		public Assignments readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Collection<Assignments> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterForApplyModel(arguments);
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterForUI(arguments);
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			/*if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilterWithAll(arguments);
			*/
			throw new RuntimeException("Not yet handled : "+arguments);
		}
		
		@Override
		public Assignments readByIdentifierForEdit(String identifier) {
			Assignments assignments = QueryExecutor.getInstance().executeReadOne(Assignments.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT).addFilterField(PARAMETER_NAME_IDENTIFIER, identifier));
			return assignments;
		}
		
		@Override
		public Collection<Assignments> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(ExecutionImputation.class, QueryName.READ_WHERE_FILTER);
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Assignments.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(ExecutionImputation.class, QueryName.COUNT_WHERE_FILTER);
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_SECTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTIVITY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ECONOMIC_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);	
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ADMINISTRATIVE_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_ACTIVITY_CATEGORY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_EXPENDITURE_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_CREDIT_MANAGER_HOLDER);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_ACCOUNTING_HOLDER);
			prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(arguments, filter, PARAMETER_NAME_ACCOUNTING_ASSISTANT);

			arguments.setFilter(filter);
		}
		
		private void prepareWhereFilterAddScopeFunctionFieldContainsStringOrWords(QueryExecutorArguments arguments,Filter filter,String parameterName) {
			filter.addFieldsNullable(arguments, parameterName);
			filter.addFieldContainsStringOrWords(parameterName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		}
	
		@Override
		public Collection<Assignments> readWhereFilterForApplyModel(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL);
			return readWhereFilter(arguments);
		}
		
		@Override
		public Collection<Assignments> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
			return readWhereFilter(arguments);
		}
	}
	
	/**/
	
	static AssignmentsQuerier getInstance() {
		return Helper.getInstance(AssignmentsQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
					, jpql(
							select("t")
							,getReadWhereFilterFromWhere()
							,order(asc("t",Assignments.FIELD_IDENTIFIER))
							))
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER
					, jpql(select("COUNT(t.identifier)"),getReadWhereFilterFromWhere()))
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL
					, jpql(
							/*String.format("SELECT new %s(t.%s.identifier,t.%s.identifier"
									+ ",t.%s.identifier,t.%s.identifier,t.%s.identifier,t.%s.identifier,t.%s.identifier,t.%s.identifier"
									+ ",t.%s.identifier,t.%s.identifier)",Assignments.class.getName()
									,Assignments.FIELD_IDENTIFIER,Assignments.FIELD_EXECUTION_IMPUTATION
									,Assignments.FIELD_CREDIT_MANAGER_HOLDER,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT
									,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT
									,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT
									,Assignments.FIELD_ACCOUNTING_HOLDER,Assignments.FIELD_ACCOUNTING_ASSISTANT
									)
							*/
							"SELECT t"
							//"SELECT t.identifier"
							,getReadWhereFilterFromWhere()
							,order(asc("t",Assignments.FIELD_IDENTIFIER))
							))
		);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(
				from(
					"Assignments t"
					,getLeftJoinScopeFunction(Assignments.FIELD_CREDIT_MANAGER_HOLDER, "gc"),getLeftJoinScopeFunction(Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, "agc")
					,getLeftJoinScopeFunction(Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, "ord"),getLeftJoinScopeFunction(Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, "aord")
					,getLeftJoinScopeFunction(Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, "cf"),getLeftJoinScopeFunction(Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, "acf")
					,getLeftJoinScopeFunction(Assignments.FIELD_ACCOUNTING_HOLDER, "cpt"),getLeftJoinScopeFunction(Assignments.FIELD_ACCOUNTING_ASSISTANT, "acpt")
				)
				,where(and(
					like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_SECTION_CODE_NAME, PARAMETER_NAME_SECTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ACTION_CODE_NAME, PARAMETER_NAME_ACTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ACTIVITY_CODE_NAME, PARAMETER_NAME_ACTIVITY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME, PARAMETER_NAME_ECONOMIC_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME, PARAMETER_NAME_ADMINISTRATIVE_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE_NAME, PARAMETER_NAME_ACTIVITY_CATEGORY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE_NAME, PARAMETER_NAME_EXPENDITURE_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
					,getReadWhereFilterScopeFunctionPredicate("gc", PARAMETER_NAME_CREDIT_MANAGER_HOLDER)
					,getReadWhereFilterScopeFunctionPredicate("agc", PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT)
					,getReadWhereFilterScopeFunctionPredicate("ord", PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER)
					,getReadWhereFilterScopeFunctionPredicate("aord", PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT)
					,getReadWhereFilterScopeFunctionPredicate("cf", PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER)
					,getReadWhereFilterScopeFunctionPredicate("acf", PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT)
					,getReadWhereFilterScopeFunctionPredicate("cpt", PARAMETER_NAME_ACCOUNTING_HOLDER)
					,getReadWhereFilterScopeFunctionPredicate("acpt", PARAMETER_NAME_ACCOUNTING_ASSISTANT)
					
				))
			);
	}
	
	static String getReadWhereFilterScopeFunctionPredicate(String holderVariableName,String holderParameterName) {
		return parenthesis(or(
				String.format("(:%sNullable = true)", holderParameterName)
				,like(holderVariableName, ScopeFunction.FIELD_CODE, holderParameterName)
				,like(holderVariableName, ScopeFunction.FIELD_NAME, holderParameterName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	}
	
	static String getLeftJoinScopeFunction(String holderFieldName,String holderVariableName) {
		return String.format("LEFT JOIN ScopeFunction %2$s ON %2$s = t.%1$s", holderFieldName,holderVariableName);
	}
}