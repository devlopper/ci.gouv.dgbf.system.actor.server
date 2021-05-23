package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.fields;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.isNotNull;
import static org.cyk.utility.persistence.query.Language.Where.isNull;
import static org.cyk.utility.persistence.query.Language.Where.isNullable;
import static org.cyk.utility.persistence.query.Language.Where.like;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.ParameterNameBuilder;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public interface AssignmentsQuerier extends Querier {

	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	String PARAMETER_NAME_SECTION = "section";
	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_SECTION_CODE = "sectionCode";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT = "administrativeUnit";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeUnitIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_CODE = "administrativeUnitCode";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "budgetSpecializationUnitIdentifier";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE = "budgetSpecializationUnitCode";
	String PARAMETER_NAME_ACTION = "action";
	String PARAMETER_NAME_ACTION_IDENTIFIER = "actionIdentifier";
	String PARAMETER_NAME_ACTION_CODE = "actionCode";
	String PARAMETER_NAME_ACTIVITY = "activity";
	String PARAMETER_NAME_ACTIVITY_IDENTIFIER = "activityIdentifier";
	String PARAMETER_NAME_ACTIVITY_CODE = "activityCode";
	String PARAMETER_NAME_ECONOMIC_NATURE = "economicNature";
	String PARAMETER_NAME_ECONOMIC_NATURE_IDENTIFIER = "economicNatureIdentifier";
	String PARAMETER_NAME_ECONOMIC_NATURE_CODE = "economicNatureCode";	
	String PARAMETER_NAME_ACTIVITY_CATEGORY = "activityCategory";
	String PARAMETER_NAME_ACTIVITY_CATEGORY_IDENTIFIER = "activityCategoryIdentifier";
	String PARAMETER_NAME_ACTIVITY_CATEGORY_CODE = "activityCategoryCode";
	String PARAMETER_NAME_EXPENDITURE_NATURE = "expenditureNature";
	String PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER = "expenditureNatureIdentifier";
	String PARAMETER_NAME_EXPENDITURE_NATURE_CODE = "expenditureNatureCode";
	
	String PARAMETER_NAME_REGION_IDENTIFIER = "regionIdentifier";
	String PARAMETER_NAME_REGION_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_REGION_IDENTIFIER, null, Boolean.TRUE);	
	String PARAMETER_NAME_DEPARTMENT_IDENTIFIER = "departmentIdentifier";
	String PARAMETER_NAME_DEPARTMENT_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_DEPARTMENT_IDENTIFIER, null, Boolean.TRUE);	
	String PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER = "subPrefectureIdentifier";
	String PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER, null, Boolean.TRUE);	
	
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER = "creditManagerHolder";
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IDENTIFIER = "creditManagerHolderIdentifier";
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_HOLDER, null, Boolean.TRUE);	
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_HOLDER, Boolean.TRUE, null);
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_HOLDER, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_HOLDER, Boolean.FALSE, null);
	String PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_HOLDER, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT = "creditManagerAssistant";
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT, null, Boolean.TRUE);	
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT, Boolean.TRUE, null);
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT, Boolean.FALSE, null);
	String PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CREDIT_MANAGER_ASSISTANT, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER = "authorizingOfficerHolder";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER = "authorizingOfficerHolderIdentifier";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER, null, Boolean.TRUE);	
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER, Boolean.TRUE, null);
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER, Boolean.FALSE, null);
	String PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT = "authorizingOfficerAssistant";
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT, null, Boolean.TRUE);	
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT, Boolean.TRUE, null);
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT, Boolean.FALSE, null);
	String PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_AUTHORIZING_OFFICER_ASSISTANT, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER = "financialControllerHolder";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER = "financialControllerHolderIdentifier";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER, null, Boolean.TRUE);	
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER, Boolean.TRUE, null);
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER, Boolean.FALSE, null);
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT = "financialControllerAssistant";
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT, null, Boolean.TRUE);	
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT, Boolean.TRUE, null);
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT, Boolean.FALSE, null);
	String PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_FINANCIAL_CONTROLLER_ASSISTANT, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_ACCOUNTING_HOLDER = "accountingHolder";
	String PARAMETER_NAME_ACCOUNTING_HOLDER_IDENTIFIER = "accountingHolderIdentifier";
	String PARAMETER_NAME_ACCOUNTING_HOLDER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_HOLDER, null, Boolean.TRUE);	
	String PARAMETER_NAME_ACCOUNTING_HOLDER_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_HOLDER, Boolean.TRUE, null);
	String PARAMETER_NAME_ACCOUNTING_HOLDER_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_HOLDER, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_ACCOUNTING_HOLDER_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_HOLDER, Boolean.FALSE, null);
	String PARAMETER_NAME_ACCOUNTING_HOLDER_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_HOLDER, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT = "accountingAssistant";
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_ASSISTANT, null, Boolean.TRUE);	
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT_IS_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_ASSISTANT, Boolean.TRUE, null);
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT_IS_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_ASSISTANT, Boolean.TRUE, Boolean.TRUE);	
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT_IS_NOT_NULL = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_ASSISTANT, Boolean.FALSE, null);
	String PARAMETER_NAME_ACCOUNTING_ASSISTANT_IS_NOT_NULL_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACCOUNTING_ASSISTANT, Boolean.FALSE, Boolean.TRUE);
	
	String PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED = "someHoldersNotDefined";
	String PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED, null, Boolean.TRUE);
	
	String PARAMETER_NAME_ALL_HOLDERS_DEFINED = "allHoldersDefined";
	String PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ALL_HOLDERS_DEFINED, null, Boolean.TRUE);
	
	Assignments readOne(QueryExecutorArguments arguments);
	Collection<Assignments> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(Assignments.class,"readByIdentifierForEdit");
	Assignments readByIdentifierForEdit(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Assignments.class,"readByIdentifierForUI");
	Assignments readByIdentifierForUI(QueryExecutorArguments arguments);
	
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
	
	String QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readFullyAssignedWhereFilter");
	Collection<Assignments> readFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER);
	Long countFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readNotFullyAssignedWhereFilter");
	Collection<Assignments> readNotFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER);
	Long countNotFullyAssignedWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readFullyAssignedWhereFilterForUI");
	Collection<Assignments> readFullyAssignedWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Assignments.class, "readNotFullyAssignedWhereFilterForUI");
	Collection<Assignments> readNotFullyAssignedWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY = QueryIdentifierBuilder.getInstance().build(Assignments.class,"readWhereFilterUsingIdentifiersOnly");
	Collection<Assignments> readWhereFilterUsingIdentifiersOnly(QueryExecutorArguments arguments);	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
	Long countWhereFilterUsingIdentifiersOnly(QueryExecutorArguments arguments);
	
	Collection<Object[]> readAllPropertiesByIdentifiers(Collection<String> identifiers);
	Collection<Object[]> readAllPropertiesByIdentifiers(String... identifiers);
	Collection<Object[]> readAllProperties(Collection<Assignments> assignmentsCollection);
	Collection<Object[]> readAllProperties(Assignments... assignmentsCollection);
	
	void clean(String actor,String functionality,String action,Date date);
	void import_(String actor,String functionality,String actionCreate,String actionUpdate,Date date);
	void importNews(String actor,String functionality,String action,Date date);
	void export(String actor,String functionality,String action,Date date);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements AssignmentsQuerier,Serializable {
		
	}
	
	/**/
	
	static AssignmentsQuerier getInstance() {
		return Helper.getInstance(AssignmentsQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(
			Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
				, jpql(getForEditSelect(),getReadWhereFilterFrom(),"WHERE t.identifier = :identifier"					
				)).setTupleFieldsNamesIndexesFromFieldsNames(getForEditTupleFieldsNamesIndexesFromFieldsNames())
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
					, jpql(select("t.identifier,t.identifier"),from("Assignments t"),where("t.identifier = :identifier")					
							)).setTupleFieldsNamesIndexesFromFieldsNames(Assignments.FIELD_IDENTIFIER,Assignments.FIELD_IDENTIFIER)
						
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER, jpql(select("t"),getReadWhereFilterFromWhere(),getOrderBy()))			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, jpql(select("COUNT(t.identifier)"),getReadWhereFilterFromWhere()))
			/*
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY
					, jpql(select("t"),getReadWhereFilterUsingIdentifiersOnlyFromWhere(),getOrder()))			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY, jpql(select("COUNT(t.identifier)"),getReadWhereFilterUsingIdentifiersOnlyFromWhere()))
			*/
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER, jpql(select("t")
					,getReadFullyAssignedWhereFilterFromWhere(Boolean.TRUE),getOrderBy()))			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_FULLY_ASSIGNED_WHERE_FILTER, jpql(select("COUNT(t.identifier)")
					,getReadFullyAssignedWhereFilterFromWhere(Boolean.TRUE)))
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER, jpql(select("t")
					,getReadFullyAssignedWhereFilterFromWhere(Boolean.FALSE),getOrderBy()))			
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_NOT_FULLY_ASSIGNED_WHERE_FILTER, jpql(select("COUNT(t.identifier)")
					,getReadFullyAssignedWhereFilterFromWhere(Boolean.FALSE)))
			
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
					,Assignments.FIELD_ACCOUNTING_HOLDER_AS_STRING,Assignments.FIELD_ACCOUNTING_ASSISTANT_AS_STRING
				)
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI
					, jpql(getReadWhereFilterForUISelect(),getReadFullyAssignedWhereFilterFromWhere(Boolean.TRUE),getOrderBy())
					)
			.setTupleFieldsNamesIndexesFromFieldsNames(getReadWhereFilterForUITupleFieldsNamesIndexesFromFieldsNames())
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_NOT_FULLY_ASSIGNED_WHERE_FILTER_FOR_UI
					, jpql(getReadWhereFilterForUISelect(),getReadFullyAssignedWhereFilterFromWhere(Boolean.FALSE),getOrderBy())
					)
			.setTupleFieldsNamesIndexesFromFieldsNames(getReadWhereFilterForUITupleFieldsNamesIndexesFromFieldsNames())
			
			,Query.buildSelect(Assignments.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT
					, jpql(getForEditSelect(),getReadWhereFilterFromWhere(),getOrderBy())
					).setTupleFieldsNamesIndexesFromFieldsNames(getForEditTupleFieldsNamesIndexesFromFieldsNames())
		);
	}
	
	/* read where filter */
	
	static String getReadWhereFilterFrom() {
		return from(
				"Assignments t"
				,"LEFT JOIN ExecutionImputation i ON i = t.executionImputation"
				
				,getLeftJoinScopeFunction(Assignments.FIELD_CREDIT_MANAGER_HOLDER, Assignments.COLUMN_CREDIT_MANAGER_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER)
				,getLeftJoinScopeFunction(Assignments.FIELD_ACCOUNTING_HOLDER, Assignments.COLUMN_ACCOUNTING_HOLDER)
				
				,"LEFT JOIN ScopeFunction AGC ON AGC = t.creditManagerAssistant"
				,"LEFT JOIN ScopeFunction AORD ON AORD = t.authorizingOfficerAssistant"
				,"LEFT JOIN ScopeFunction ACF ON ACF = t.financialControllerAssistant"
				,"LEFT JOIN ScopeFunction ACPT ON ACPT = t.accountingAssistant"
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
			
			,parenthesis(or(isNullable(PARAMETER_NAME_REGION_IDENTIFIER_NULLABLE),"t.executionImputation.regionIdentifier = :regionIdentifier"))
			,parenthesis(or(isNullable(PARAMETER_NAME_DEPARTMENT_IDENTIFIER_NULLABLE),"t.executionImputation.departmentIdentifier = :departmentIdentifier"))
			,parenthesis(or(isNullable(PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER_NULLABLE),"t.executionImputation.subPrefectureIdentifier = :subPrefectureIdentifier"))
			
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_CREDIT_MANAGER_HOLDER, PARAMETER_NAME_CREDIT_MANAGER_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER, PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER, PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER)
			,getReadWhereFilterScopeFunctionPredicate(Assignments.COLUMN_ACCOUNTING_HOLDER, PARAMETER_NAME_ACCOUNTING_HOLDER)
			
			,parenthesis(or(isNullable(PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE)
					, parenthesis(and(isNotNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
							,isNotNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
							,isNotNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
							,isNotNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
							)))
				)
			
			,parenthesis(or(isNullable(PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE)
					, or(isNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
							,isNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
							,isNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
							,isNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
							))
				)
		);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(getReadWhereFilterFrom(),where(getReadWhereFilterWherePredicate()));
	}
	
	static String getReadWhereFilterScopeFunctionPredicate(String variableName,String parameterName) {
		return parenthesis(and(
				parenthesis(or(
				String.format("(:%sNullable = true)", parameterName)
				,like(variableName, ScopeFunction.FIELD_CODE, parameterName)
				,like(variableName, ScopeFunction.FIELD_NAME, parameterName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				))
				,parenthesis(or(String.format("(:%s = true)", ParameterNameBuilder.getInstance().build(parameterName, Boolean.TRUE, Boolean.TRUE)),variableName+" IS NULL"))
				,parenthesis(or(String.format("(:%s = true)", ParameterNameBuilder.getInstance().build(parameterName, Boolean.FALSE, Boolean.TRUE)),variableName+" IS NOT NULL"))
			));
	}
	
	/* Read where filter using identifiers only*/
	

	
	/* read fully assigned where filter */
	
	static String getReadFullyAssignedWhereFilterFromWhere(Boolean isFullyAssigned) {
		return jpql(getReadWhereFilterFrom(),where(getReadFullyAssignedWhereFilterWherePredicate(isFullyAssigned)));
	}
	
	static String getReadFullyAssignedWhereFilterWherePredicate(Boolean isFullyAssigned) {
		String predicate;
		if(Boolean.TRUE.equals(isFullyAssigned))
			predicate = Language.Where.allNotNull("t",Assignments.FIELDS_SCOPES_FUNCTIONS);
		else
			predicate = Language.Where.oneNull("t",Assignments.FIELDS_SCOPES_FUNCTIONS);
		return and(getReadWhereFilterWherePredicate(),parenthesis(predicate));
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
	
	static String getReadWhereFilterForUISelect() {
		return select(
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
			);
	}
	
	static String[] getReadWhereFilterForUITupleFieldsNamesIndexesFromFieldsNames() {
		return new String[] {
				Assignments.FIELD_IDENTIFIER,Assignments.FIELD_SECTION_AS_STRING,Assignments.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING
				,Assignments.FIELD_ACTION_AS_STRING,Assignments.FIELD_ACTIVITY_AS_STRING,Assignments.FIELD_ACTIVITY_CATEGORY_AS_STRING
				,Assignments.FIELD_EXPENDITURE_NATURE_AS_STRING,Assignments.FIELD_ECONOMIC_NATURE_AS_STRING,Assignments.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
				,Assignments.FIELD_CREDIT_MANAGER_HOLDER_AS_STRING,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT_AS_STRING
				,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT_AS_STRING
				,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT_AS_STRING
				,Assignments.FIELD_ACCOUNTING_HOLDER_AS_STRING,Assignments.FIELD_ACCOUNTING_ASSISTANT_AS_STRING	
		};
	}

	static String getOrderBy() {
		return order(
				asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_SECTION_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ACTION_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ACTIVITY_CODE))
				,asc("t",FieldHelper.join(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE))
			);
	}
}