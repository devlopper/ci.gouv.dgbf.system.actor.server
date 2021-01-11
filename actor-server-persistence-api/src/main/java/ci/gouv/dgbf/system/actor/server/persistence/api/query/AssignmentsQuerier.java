package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.like;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Language;
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
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readWhereFilterForEdit");
	Collection<Assignments> readWhereFilterForEdit(QueryExecutorArguments arguments);
	/*
	String QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readFullyAssignedWhereFilter");
	Collection<Assignments> readFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER);
	Long countFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readNotFullyAssignedWhereFilter");
	Collection<Assignments> readNotFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER);
	Long countNotFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	*/
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
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterForEdit(arguments);
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
				arguments = QueryExecutorArguments.instantiate(Assignments.class, QueryName.READ_WHERE_FILTER);
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Assignments.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(Assignments.class, QueryName.COUNT_WHERE_FILTER);
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
		
		@Override
		public Collection<Assignments> readWhereFilterForEdit(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT);
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
			Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
				, jpql(getForEditSelect(),getReadWhereFilterFrom(),"WHERE t.identifier = :identifier"					
				)).setTupleFieldsNamesIndexesFromFieldsNames(getForEditTupleFieldsNamesIndexesFromFieldsNames())
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER, jpql(select("t"),getReadWhereFilterFromWhere(),getOrderBy()))
			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, jpql(select("COUNT(t.identifier)"),getReadWhereFilterFromWhere()))
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_APPLY_MODEL, jpql("SELECT t",getReadWhereFilterFromWhere(),getOrderBy()))
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI
					, jpql(
							select(
									fields("t",Assignments.FIELD_IDENTIFIER)
									,fields("t."+Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_SECTION_CODE_NAME
											,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME,ExecutionImputation.FIELD_ACTION_CODE_NAME
											,ExecutionImputation.FIELD_ACTIVITY_CODE_NAME,ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE_NAME
											,ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE_NAME,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME
											,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME)
									,Language.Select.concatCodeName(
											Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT
											,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT
											,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
											,Assignments.COLUMN_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_ASSISTANT
											)
								)
							,getReadWhereFilterFromWhere()
							,getOrderBy()
							))
			.setTupleFieldsNamesIndexesFromFieldsNames(Assignments.FIELD_IDENTIFIER,Assignments.FIELD_SECTION_AS_STRING,Assignments.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING
					,Assignments.FIELD_ACTION_AS_STRING,Assignments.FIELD_ACTIVITY_AS_STRING,Assignments.FIELD_ACTIVITY_CATEGORY_AS_STRING
					,Assignments.FIELD_EXPENDITURE_NATURE_AS_STRING,Assignments.FIELD_ECONOMIC_NATURE_AS_STRING,Assignments.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
					,Assignments.FIELD_CREDIT_MANAGER_HOLDER_AS_STRING,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT_AS_STRING
					,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT_AS_STRING
					,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT_AS_STRING
					,Assignments.FIELD_ACCOUNTING_HOLDER_AS_STRING,Assignments.FIELD_ACCOUNTING_ASSISTANT_AS_STRING)
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT
					, jpql(getForEditSelect(),getReadWhereFilterFromWhere(),getOrderBy())
					).setTupleFieldsNamesIndexesFromFieldsNames(getForEditTupleFieldsNamesIndexesFromFieldsNames())
		);
	}
	
	/* read where filter */
	
	static String getReadWhereFilterFrom() {
		return from(
				"Assignments t"
				,getLeftJoinScopeFunction(Assignments.FIELD_CREDIT_MANAGER_HOLDER, Assignments.COLUMN_CREDIT_MANAGER_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT)
				,getLeftJoinScopeFunction(Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT)
				,getLeftJoinScopeFunction(Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT)
				,getLeftJoinScopeFunction(Assignments.FIELD_ACCOUNTING_HOLDER, Assignments.COLUMN_ACCOUNTING_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_ACCOUNTING_ASSISTANT, Assignments.COLUMN_ACCOUNTING_ASSISTANT)
			);
	}
	
	static String getReadWhereFilterWherePredicate() {
		return and(
			like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_SECTION_CODE_NAME, PARAMETER_NAME_SECTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ACTION_CODE_NAME, PARAMETER_NAME_ACTION, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ACTIVITY_CODE_NAME, PARAMETER_NAME_ACTIVITY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME, PARAMETER_NAME_ECONOMIC_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME, PARAMETER_NAME_ADMINISTRATIVE_UNIT, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE_NAME, PARAMETER_NAME_ACTIVITY_CATEGORY, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,like("t."+Assignments.FIELD_EXECUTION_IMPUTATION, ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE_NAME, PARAMETER_NAME_EXPENDITURE_NATURE, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_CREDIT_MANAGER_HOLDER, PARAMETER_NAME_CREDIT_MANAGER_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT, PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER, PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT, PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER, PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT, PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_ACCOUNTING_HOLDER, PARAMETER_NAME_ACCOUNTING_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_ACCOUNTING_ASSISTANT, PARAMETER_NAME_ACCOUNTING_ASSISTANT)				
		);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(getReadWhereFilterFrom(),where(getReadWhereFilterWherePredicate()));
	}
	
	static String getReadWhereFilterScopeFunctionPredicate(String holderVariableName,String holderParameterName) {
		return parenthesis(or(
				String.format("(:%sNullable = true)", holderParameterName)
				,like(holderVariableName, ScopeFunction.FIELD_CODE, holderParameterName)
				,like(holderVariableName, ScopeFunction.FIELD_NAME, holderParameterName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	}
	
	/* read fully assigned where filter */
	
	static String getReadFullyAssignedWhereFilterWherePredicate(Boolean isFullyAssigned) {
		String fullyAssigned;
		if(Boolean.TRUE.equals(isFullyAssigned))
			fullyAssigned = String.format("("+StringUtils.repeat("t.%s IS NOT NULL", 8)+")"
					,Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT
					,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT
					,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
					,Assignments.COLUMN_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_ASSISTANT
					);
		else
			fullyAssigned = String.format("("+StringUtils.repeat("t.%s IS NULL", 8)+")"
					,Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT
					,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT
					,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
					,Assignments.COLUMN_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_ASSISTANT
					);
		return and(getReadWhereFilterWherePredicate(),fullyAssigned);
	}
	
	/* read not fully assigned where filter */
	
	
	
	static String getLeftJoinScopeFunction(String holderFieldName,String holderVariableName) {
		return String.format("LEFT JOIN ScopeFunction %2$s ON %2$s = t.%1$s", holderFieldName,holderVariableName);
	}
	
	static String getForEditSelect() {
		return select(
				fields("t",Assignments.FIELD_IDENTIFIER)
				,fields("t."+Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_SECTION_CODE_NAME
					,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME,ExecutionImputation.FIELD_ACTION_CODE_NAME
					,ExecutionImputation.FIELD_ACTIVITY_CODE_NAME,ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE_NAME
					,ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE_NAME,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE_NAME
					,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME)
				,Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT
						,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT
						,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
						,Assignments.COLUMN_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_ASSISTANT
			);
	}
	
	static String[] getForEditTupleFieldsNamesIndexesFromFieldsNames() {
		return new String[] {Assignments.FIELD_IDENTIFIER,Assignments.FIELD_SECTION_AS_STRING,Assignments.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING
						,Assignments.FIELD_ACTION_AS_STRING,Assignments.FIELD_ACTIVITY_AS_STRING,Assignments.FIELD_ACTIVITY_CATEGORY_AS_STRING
						,Assignments.FIELD_EXPENDITURE_NATURE_AS_STRING,Assignments.FIELD_ECONOMIC_NATURE_AS_STRING,Assignments.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
						,Assignments.FIELD_CREDIT_MANAGER_HOLDER,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT
						,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT
						,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT
						,Assignments.FIELD_ACCOUNTING_HOLDER,Assignments.FIELD_ACCOUNTING_ASSISTANT};
	}
	
	static String getOrderBy() {
		return order(
				asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_SECTION_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ACTION_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ACTIVITY_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE))
			);
	}
}