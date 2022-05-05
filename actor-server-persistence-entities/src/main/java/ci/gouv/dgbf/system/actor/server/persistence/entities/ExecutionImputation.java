package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.NamedStoredProcedureQueries;
import javax.persistence.NamedStoredProcedureQuery;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ExecutionImputation.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
@NamedStoredProcedureQueries(value = {
	@NamedStoredProcedureQuery(
		name = ExecutionImputation.PROCEDURE_REFRESH_MATERIALIZED_VIEW, 
		procedureName = ExecutionImputation.PROCEDURE_NAME_REFRESH_MATERIALIZED_VIEW
	)
})
@NamedQueries(value = {
		@NamedQuery(name = ExecutionImputation.QUERY_READ_EXERCISE_YEAR_AND_ACTIVITY_IDENTIFIER_BY_ASSIGNMENTS_IDENTIFIER,query = 
				"SELECT t.exercise,t.activityIdentifier FROM ExecutionImputation t JOIN Assignments a ON a.executionImputation = t WHERE a.identifier = :assignmentsIdentifier")
})
public class ExecutionImputation extends AbstractImputation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_EXERCISE) private Integer exercise;
	
	/*Gestionnaire de crédits*/
	
	@Transient private String creditManagerHolderScopeFunctionExecutionImputationIdentifier;
	@Transient private String creditManagerHolderScopeFunctionIdentifier;
	@Transient private String creditManagerHolderScopeFunctionCodeName;
	
	@Transient private String creditManagerAssistantScopeFunctionExecutionImputationIdentifier;
	@Transient private String creditManagerAssistantScopeFunctionIdentifier;
	@Transient private String creditManagerAssistantScopeFunctionCodeName;
	
	/*Ordonnateur*/
	
	@Transient private String authorizingOfficerHolderScopeFunctionExecutionImputationIdentifier;
	@Transient private String authorizingOfficerHolderScopeFunctionIdentifier;
	@Transient private String authorizingOfficerHolderScopeFunctionCodeName;
	
	@Transient private String authorizingOfficerAssistantScopeFunctionExecutionImputationIdentifier;
	@Transient private String authorizingOfficerAssistantScopeFunctionIdentifier;
	@Transient private String authorizingOfficerAssistantScopeFunctionCodeName;
	
	/*Contrôleur financier*/
	
	@Transient private String financialControllerHolderScopeFunctionExecutionImputationIdentifier;
	@Transient private String financialControllerHolderScopeFunctionIdentifier;
	@Transient private String financialControllerHolderScopeFunctionCodeName;
	
	@Transient private String financialControllerAssistantScopeFunctionExecutionImputationIdentifier;
	@Transient private String financialControllerAssistantScopeFunctionIdentifier;
	@Transient private String financialControllerAssistantScopeFunctionCodeName;
	
	/*Comptable*/
	
	@Transient private String accountingHolderScopeFunctionExecutionImputationIdentifier;
	@Transient private String accountingHolderScopeFunctionIdentifier;
	@Transient private String accountingHolderScopeFunctionCodeName;
	
	@Transient private String accountingAssistantScopeFunctionExecutionImputationIdentifier;
	@Transient private String accountingAssistantScopeFunctionIdentifier;
	@Transient private String accountingAssistantScopeFunctionCodeName;
	
	@Transient private ExecutionImputationScopeFunction creditManager;
	@Transient private ExecutionImputationScopeFunction authorizingOfficer;
	@Transient private ExecutionImputationScopeFunction financialController;
	@Transient private ExecutionImputationScopeFunction accounting;
	
	@Transient private Collection<Function> functions;
	
	@Override
	public ExecutionImputation setIdentifier(String identifier) {
		return (ExecutionImputation) super.setIdentifier(identifier);
	}
	
	public ExecutionImputationScopeFunction getCreditManager(Boolean instantiateIfNull) {
		if(creditManager == null && Boolean.TRUE.equals(instantiateIfNull))
			creditManager = new ExecutionImputationScopeFunction();
		return creditManager;
	}
	
	public ExecutionImputation setCreditManagerHolderIdentifier(String identifier) {
		getCreditManager(Boolean.TRUE).setHolderIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setCreditManagerHolder(ScopeFunction scopeFunction) {
		getCreditManager(Boolean.TRUE).setHolder(scopeFunction);
		return this;
	}
	
	public String getCreditManagerHolderIdentifier() {
		return creditManager == null ? null : creditManager.getHolderIdentifier();
	}
	
	public ExecutionImputation setCreditManagerAssistantIdentifier(String identifier) {
		getCreditManager(Boolean.TRUE).setAssistantIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setCreditManagerAssistant(ScopeFunction scopeFunction) {
		getCreditManager(Boolean.TRUE).setAssistant(scopeFunction);
		return this;
	}
	
	public String getCreditManagerAssistantIdentifier() {
		return creditManager == null ? null : creditManager.getAssistantIdentifier();
	}
	
	public ExecutionImputationScopeFunction getAuthorizingOfficer(Boolean instantiateIfNull) {
		if(authorizingOfficer == null && Boolean.TRUE.equals(instantiateIfNull))
			authorizingOfficer = new ExecutionImputationScopeFunction();
		return authorizingOfficer;
	}
	
	public String getAuthorizingOfficerHolderIdentifier() {
		return authorizingOfficer == null ? null : authorizingOfficer.getHolderIdentifier();
	}
	
	public ExecutionImputation setAuthorizingOfficerHolderIdentifier(String identifier) {
		getAuthorizingOfficer(Boolean.TRUE).setHolderIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setAuthorizingOfficerHolder(ScopeFunction scopeFunction) {
		getAuthorizingOfficer(Boolean.TRUE).setHolder(scopeFunction);
		return this;
	}
	
	public ExecutionImputation setAuthorizingOfficerAssistantIdentifier(String identifier) {
		getAuthorizingOfficer(Boolean.TRUE).setAssistantIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setAuthorizingOfficerAssistant(ScopeFunction scopeFunction) {
		getAuthorizingOfficer(Boolean.TRUE).setAssistant(scopeFunction);
		return this;
	}
	
	public ExecutionImputationScopeFunction getFinancialController(Boolean instantiateIfNull) {
		if(financialController == null && Boolean.TRUE.equals(instantiateIfNull))
			financialController = new ExecutionImputationScopeFunction();
		return financialController;
	}
	
	public String getFinancialControllerHolderIdentifier() {
		return financialController == null ? null : financialController.getHolderIdentifier();
	}
	
	public ExecutionImputation setFinancialControllerHolderIdentifier(String identifier) {
		getFinancialController(Boolean.TRUE).setHolderIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setFinancialControllerHolder(ScopeFunction scopeFunction) {
		getFinancialController(Boolean.TRUE).setHolder(scopeFunction);
		return this;
	}
	
	public ExecutionImputation setFinancialControllerAssistantIdentifier(String identifier) {
		getFinancialController(Boolean.TRUE).setAssistantIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setFinancialControllerAssistant(ScopeFunction scopeFunction) {
		getFinancialController(Boolean.TRUE).setAssistant(scopeFunction);
		return this;
	}
	
	public ExecutionImputationScopeFunction getAccounting(Boolean instantiateIfNull) {
		if(accounting == null && Boolean.TRUE.equals(instantiateIfNull))
			accounting = new ExecutionImputationScopeFunction();
		return accounting;
	}
	
	public String getAccountingHolderIdentifier() {
		return accounting == null ? null : accounting.getHolderIdentifier();
	}
	
	public ExecutionImputation setAccountingHolderIdentifier(String identifier) {
		getAccounting(Boolean.TRUE).setHolderIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setAccountingHolder(ScopeFunction scopeFunction) {
		getAccounting(Boolean.TRUE).setHolder(scopeFunction);
		return this;
	}
	
	public ExecutionImputation setAccountingAssistantIdentifier(String identifier) {
		getAccounting(Boolean.TRUE).setAssistantIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setAccountingAssistant(ScopeFunction scopeFunction) {
		getAccounting(Boolean.TRUE).setAssistant(scopeFunction);
		return this;
	}
	
	public Collection<String> getScopeFunctionExecutionImputationIdentifiers() {
		Collection<String> scopeFunctionExecutionImputationIdentifiers = null;
		scopeFunctionExecutionImputationIdentifiers = addScopeFunctionExecutionImputationIdentifier(creditManagerHolderScopeFunctionExecutionImputationIdentifier, scopeFunctionExecutionImputationIdentifiers);
		scopeFunctionExecutionImputationIdentifiers = addScopeFunctionExecutionImputationIdentifier(authorizingOfficerHolderScopeFunctionExecutionImputationIdentifier, scopeFunctionExecutionImputationIdentifiers);
		scopeFunctionExecutionImputationIdentifiers = addScopeFunctionExecutionImputationIdentifier(financialControllerHolderScopeFunctionExecutionImputationIdentifier, scopeFunctionExecutionImputationIdentifiers);
		scopeFunctionExecutionImputationIdentifiers = addScopeFunctionExecutionImputationIdentifier(accountingHolderScopeFunctionExecutionImputationIdentifier, scopeFunctionExecutionImputationIdentifiers);
		return scopeFunctionExecutionImputationIdentifiers;
	}
	
	public Collection<String> addScopeFunctionExecutionImputationIdentifier(String identifier,Collection<String> scopeFunctionExecutionImputationIdentifiers) {
		if(StringHelper.isNotBlank(identifier)) {
			if(scopeFunctionExecutionImputationIdentifiers == null)
				scopeFunctionExecutionImputationIdentifiers = new ArrayList<>();
			scopeFunctionExecutionImputationIdentifiers.add(identifier);
		}
		return scopeFunctionExecutionImputationIdentifiers;
	}
	
	public Collection<String> getScopeFunctionIdentifiers() {
		Collection<String> scopeFunctionIdentifiers = null;
		scopeFunctionIdentifiers = addScopeFunctionIdentifier(creditManagerHolderScopeFunctionIdentifier, scopeFunctionIdentifiers);
		scopeFunctionIdentifiers = addScopeFunctionIdentifier(authorizingOfficerHolderScopeFunctionIdentifier, scopeFunctionIdentifiers);
		scopeFunctionIdentifiers = addScopeFunctionIdentifier(financialControllerHolderScopeFunctionIdentifier, scopeFunctionIdentifiers);
		scopeFunctionIdentifiers = addScopeFunctionIdentifier(accountingHolderScopeFunctionIdentifier, scopeFunctionIdentifiers);
		return scopeFunctionIdentifiers;
	}
	
	public Collection<String> addScopeFunctionIdentifier(String identifier,Collection<String> scopeFunctionIdentifiers) {
		if(StringHelper.isNotBlank(identifier)) {
			if(scopeFunctionIdentifiers == null)
				scopeFunctionIdentifiers = new ArrayList<>();
			scopeFunctionIdentifiers.add(identifier);
		}
		return scopeFunctionIdentifiers;
	}
	
	/*
	public Collection<ScopeFunction> computeScopeFunctions() {
		Collection<ScopeFunction> scopeFunctions = null;
		for(ExecutionImputationScopeFunction executionImputationScopeFunction : new ExecutionImputationScopeFunction[] {creditManager,authorizingOfficer,financialController,accounting}) {
			if(executionImputationScopeFunction == null || (executionImputationScopeFunction.getHolder() == null && executionImputationScopeFunction.getAssistant() == null))
				continue;
			if(scopeFunctions == null)
				scopeFunctions = new ArrayList<>();
			if(executionImputationScopeFunction.getHolder() != null)
				scopeFunctions.add(executionImputationScopeFunction.getHolder());
			if(executionImputationScopeFunction.getAssistant() != null)
				scopeFunctions.add(executionImputationScopeFunction.getAssistant());
		}
		return scopeFunctions;
	}
	*/
	public Boolean hasScopeFunction(ScopeFunction scopeFunction) {
		if(scopeFunction == null || StringHelper.isBlank(scopeFunction.getIdentifier()))
			return Boolean.FALSE;
		if(scopeFunction.getIdentifier().equals(creditManagerHolderScopeFunctionIdentifier) || scopeFunction.getIdentifier().equals(authorizingOfficerHolderScopeFunctionIdentifier)
				 || scopeFunction.getIdentifier().equals(financialControllerHolderScopeFunctionIdentifier) || scopeFunction.getIdentifier().equals(accountingHolderScopeFunctionIdentifier))
			return Boolean.TRUE;
		return Boolean.FALSE;
	}
	
	/**/
	
	//ScopeFunction
	
	private static final String SCOPE_FUNCTION_IDENTIFIER_FIELD_NAME_FORMAT = "%s%sScopeFunctionIdentifier";
	public static String buildScopeFunctionIdentifierFieldName(String functionFieldName,String type) {
		return String.format(SCOPE_FUNCTION_IDENTIFIER_FIELD_NAME_FORMAT, functionFieldName,type);
	}
	public static String buildScopeFunctionHolderIdentifierFieldName(String functionFieldName) {
		return buildScopeFunctionIdentifierFieldName(functionFieldName,"Holder");
	}
	public static String buildScopeFunctionAssistantIdentifierFieldName(String functionFieldName) {
		return buildScopeFunctionIdentifierFieldName(functionFieldName,"Assistant");
	}
	
	private static final String SCOPE_FUNCTION_IDENTIFIER_COLUMN_NAME_FORMAT = "%s_IDENTIFIANT";
	public static String buildScopeFunctionIdentifierColumnName(String functionColumnName) {
		return String.format(SCOPE_FUNCTION_IDENTIFIER_COLUMN_NAME_FORMAT, functionColumnName);
	}
	
	private static final String SCOPE_FUNCTION_CODE_NAME_FIELD_NAME_FORMAT = "%s%sScopeFunctionCodeName";
	public static String buildScopeFunctionCodeNameFieldName(String functionFieldName,String type) {
		return String.format(SCOPE_FUNCTION_CODE_NAME_FIELD_NAME_FORMAT, functionFieldName,type);
	}	
	public static String buildScopeFunctionHolderCodeNameFieldName(String functionFieldName) {
		return buildScopeFunctionCodeNameFieldName(functionFieldName, "Holder");
	}
	public static String buildScopeFunctionAssistantCodeNameFieldName(String functionFieldName) {
		return buildScopeFunctionCodeNameFieldName(functionFieldName, "Assistant");
	}
	
	private static final String SCOPE_FUNCTION_CODE_NAME_COLUMN_NAME_FORMAT = "%s_CODE_LIBELLE";
	public static String buildScopeFunctionCodeNameColumnName(String functionColumnName) {
		return String.format(SCOPE_FUNCTION_CODE_NAME_COLUMN_NAME_FORMAT, functionColumnName);
	}
	
	//ScopeFunctionExecutionImputation
	
	private static final String SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER_FIELD_NAME_FORMAT = "%s%sScopeFunctionExecutionImputationIdentifier";
	public static String buildScopeFunctionExecutionImputationIdentifierFieldName(String scopeFunctionFieldName,String type) {
		return String.format(SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER_FIELD_NAME_FORMAT, scopeFunctionFieldName,type);
	}
	public static String buildScopeFunctionExecutionImputationHolderIdentifierFieldName(String scopeFunctionFieldName) {
		return buildScopeFunctionExecutionImputationIdentifierFieldName(scopeFunctionFieldName, "Holder");
	}
	public static String buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(String scopeFunctionFieldName) {
		return buildScopeFunctionExecutionImputationIdentifierFieldName(scopeFunctionFieldName, "Assistant");
	}
	
	private static final String SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER_COLUMN_NAME_FORMAT = "%s_A_IDENTIFIANT";
	public static String buildScopeFunctionExecutionImputationIdentifierColumnName(String scopeFunctionColumnName) {
		return String.format(SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER_COLUMN_NAME_FORMAT, scopeFunctionColumnName);
	}
	
	public static final String FIELD_EXERCISE = "exercise";
	
	public static final String FIELD_CREDIT_MANAGER = "creditManager";
	//Holder
	public static final String FIELD_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(FIELD_CREDIT_MANAGER);
	public static final String FIELD_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(FIELD_CREDIT_MANAGER);
	public static final String FIELD_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(FIELD_CREDIT_MANAGER);
	//Assistant
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(FIELD_CREDIT_MANAGER);
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(FIELD_CREDIT_MANAGER);
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(FIELD_CREDIT_MANAGER);
	
	public static final String FIELD_AUTHORIZING_OFFICER = "authorizingOfficer";
	//Holder
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(FIELD_AUTHORIZING_OFFICER);
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(FIELD_AUTHORIZING_OFFICER);
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(FIELD_AUTHORIZING_OFFICER);
	//Assistant
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(FIELD_AUTHORIZING_OFFICER);
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(FIELD_AUTHORIZING_OFFICER);
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(FIELD_AUTHORIZING_OFFICER);
	
	public static final String FIELD_FINANCIAL_CONTROLLER = "financialController";
	//Holder
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(FIELD_FINANCIAL_CONTROLLER);
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(FIELD_FINANCIAL_CONTROLLER);
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(FIELD_FINANCIAL_CONTROLLER);
	//Assistant
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(FIELD_FINANCIAL_CONTROLLER);
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(FIELD_FINANCIAL_CONTROLLER);
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(FIELD_FINANCIAL_CONTROLLER);
	
	public static final String FIELD_ACCOUNTING = "accounting";
	//Holder
	public static final String FIELD_ACCOUNTING_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(FIELD_ACCOUNTING);
	public static final String FIELD_ACCOUNTING_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(FIELD_ACCOUNTING);
	public static final String FIELD_ACCOUNTING_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(FIELD_ACCOUNTING);
	//Assistant
	public static final String FIELD_ACCOUNTING_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(FIELD_ACCOUNTING);
	public static final String FIELD_ACCOUNTING_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(FIELD_ACCOUNTING);
	public static final String FIELD_ACCOUNTING_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(FIELD_ACCOUNTING);
	
	public static final String TABLE_NAME = "VM_APP_EX_IMPUTATION";
	public static final String PROCEDURE_NAME_REFRESH_MATERIALIZED_VIEW = "P_RAFFRAICHIR_VM_APP_EX_IMP";
	//public static final String VIEW_NAME = "VM_APP_EX_IMPUTATION_POSTE";
	
	public static final String COLUMN_EXERCISE = "exercice";
	
	//Holder
	public static final String COLUMN_CREDIT_MANAGER_HOLDER = "GC";
	public static final String COLUMN_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionIdentifierColumnName(COLUMN_CREDIT_MANAGER_HOLDER);
	public static final String COLUMN_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionCodeNameColumnName(COLUMN_CREDIT_MANAGER_HOLDER);
	public static final String COLUMN_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationIdentifierColumnName(COLUMN_CREDIT_MANAGER_HOLDER);
	//Assistant
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT = "AGC";
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionIdentifierColumnName(COLUMN_CREDIT_MANAGER_ASSISTANT);
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionCodeNameColumnName(COLUMN_CREDIT_MANAGER_ASSISTANT);
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationIdentifierColumnName(COLUMN_CREDIT_MANAGER_ASSISTANT);
	
	//Holder
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER = "ORD";
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(COLUMN_AUTHORIZING_OFFICER_HOLDER);
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(COLUMN_AUTHORIZING_OFFICER_HOLDER);
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(COLUMN_AUTHORIZING_OFFICER_HOLDER);
	//Assistant
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT = "AORD";
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(COLUMN_AUTHORIZING_OFFICER_ASSISTANT);
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(COLUMN_AUTHORIZING_OFFICER_ASSISTANT);
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(COLUMN_AUTHORIZING_OFFICER_ASSISTANT);
	
	//Holder
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER = "CF";
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(COLUMN_FINANCIAL_CONTROLLER_HOLDER);
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(COLUMN_FINANCIAL_CONTROLLER_HOLDER);
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(COLUMN_FINANCIAL_CONTROLLER_HOLDER);
	//Assistant
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT = "ACF";
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(COLUMN_FINANCIAL_CONTROLLER_ASSISTANT);
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(COLUMN_FINANCIAL_CONTROLLER_ASSISTANT);
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(COLUMN_FINANCIAL_CONTROLLER_ASSISTANT);
	
	//Holder
	public static final String COLUMN_ACCOUNTING_HOLDER = "CPT";
	public static final String COLUMN_ACCOUNTING_HOLDER_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionHolderIdentifierFieldName(COLUMN_ACCOUNTING_HOLDER);
	public static final String COLUMN_ACCOUNTING_HOLDER_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionHolderCodeNameFieldName(COLUMN_ACCOUNTING_HOLDER);
	public static final String COLUMN_ACCOUNTING_HOLDER_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationHolderIdentifierFieldName(COLUMN_ACCOUNTING_HOLDER);
	//Assistant
	public static final String COLUMN_ACCOUNTING_ASSISTANT = "ACPT";
	public static final String COLUMN_ACCOUNTING_ASSISTANT_SCOPE_FUNCTION_IDENTIFIER = buildScopeFunctionAssistantIdentifierFieldName(COLUMN_ACCOUNTING_ASSISTANT);
	public static final String COLUMN_ACCOUNTING_ASSISTANT_SCOPE_FUNCTION_CODE_NAME = buildScopeFunctionAssistantCodeNameFieldName(COLUMN_ACCOUNTING_ASSISTANT);
	public static final String COLUMN_ACCOUNTING_ASSISTANT_SCOPE_FUNCTION_EXECUTION_IMPUTATION_IDENTIFIER = buildScopeFunctionExecutionImputationAssistantIdentifierFieldName(COLUMN_ACCOUNTING_ASSISTANT);
	
	public static final String PROCEDURE_REFRESH_MATERIALIZED_VIEW = "ExecutionImputation.refreshMaterializedView";
	
	public static final Collection<String> FUNCTIONS_FIELDS_NAMES = List.of(FIELD_CREDIT_MANAGER,FIELD_AUTHORIZING_OFFICER,FIELD_FINANCIAL_CONTROLLER,FIELD_ACCOUNTING);
	
	public static final String FUNCTION_FIELD_NAME_TYPE_HOLDER = "Holder";
	public static final String FUNCTION_FIELD_NAME_TYPE_ASSISTANT = "Assistant";
	public static final Collection<String> FUNCTIONS_FIELDS_NAMES_TYPES = List.of(FUNCTION_FIELD_NAME_TYPE_HOLDER,FUNCTION_FIELD_NAME_TYPE_ASSISTANT);
	
	public static final String QUERY_READ_EXERCISE_YEAR_AND_ACTIVITY_IDENTIFIER_BY_ASSIGNMENTS_IDENTIFIER = "ExecutionImputation.readExerciseYearAndActivityIdentifierByAssignmentsIdentifier";
}