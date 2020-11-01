package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.NamedStoredProcedureQuery;
import javax.persistence.SecondaryTable;
import javax.persistence.SecondaryTables;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ExecutionImputation.TABLE_NAME)
@SecondaryTables(value= @SecondaryTable(name = ExecutionImputation.VIEW_NAME))
@NamedStoredProcedureQuery(
	name = ExecutionImputation.PROCEDURE_REFRESH_MATERIALIZED_VIEW, 
	procedureName = "PROC_REFRESH_VM_APP_EX_IP"
)
public class ExecutionImputation extends AbstractImputation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(table = VIEW_NAME,name = "GC_A_IDENTIFIANT")
	private String creditManagerHolderScopeFunctionExecutionImputationIdentifier;
	@Column(table = VIEW_NAME,name = "GC_P_IDENTIFIANT")
	private String creditManagerHolderScopeFunctionIdentifier;
	@Column(table = VIEW_NAME,name = "GC_P_CODE_LIBELLE")
	private String creditManagerHolderScopeFunctionCodeName;
	//private String creditManagerAssistantCodeName;
	
	@Column(table = VIEW_NAME,name = "ORD_A_IDENTIFIANT")
	private String authorizingOfficerHolderScopeFunctionExecutionImputationIdentifier;
	@Column(table = VIEW_NAME,name = "ORD_P_IDENTIFIANT")
	private String authorizingOfficerHolderScopeFunctionIdentifier;
	@Column(table = VIEW_NAME,name = "ORD_P_CODE_LIBELLE")
	private String authorizingOfficerHolderScopeFunctionCodeName;
	//private String assistantCodeName;
	
	@Column(table = VIEW_NAME,name = "CF_A_IDENTIFIANT")
	private String financialControllerHolderScopeFunctionExecutionImputationIdentifier;
	@Column(table = VIEW_NAME,name = "CF_P_IDENTIFIANT")
	private String financialControllerHolderScopeFunctionIdentifier;
	@Column(table = VIEW_NAME,name = "CF_P_CODE_LIBELLE")
	private String financialControllerHolderScopeFunctionCodeName;
	//private String assistantCodeName;
	
	@Column(table = VIEW_NAME,name = "CPT_A_IDENTIFIANT")
	private String accountingHolderScopeFunctionExecutionImputationIdentifier;
	@Column(table = VIEW_NAME,name = "CPT_P_IDENTIFIANT")
	private String accountingHolderScopeFunctionIdentifier;
	@Column(table = VIEW_NAME,name = "CPT_P_CODE_LIBELLE")
	private String accountingHolderScopeFunctionCodeName;
	//private String assistantCodeName;
	
	@Transient private ExecutionImputationScopeFunction creditManager;
	@Transient private ExecutionImputationScopeFunction authorizingOfficer;
	@Transient private ExecutionImputationScopeFunction financialController;
	@Transient private ExecutionImputationScopeFunction accounting;
	
	@Transient private Collection<Function> functions;
	
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
	public static final String VIEW_NAME = "VM_APP_EX_IMPUTATION_POSTE";
	
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
	public static final Collection<String> FUNCTIONS_FIELDS_NAMES_TYPES = List.of(FUNCTION_FIELD_NAME_TYPE_HOLDER/*,FUNCTION_FIELD_NAME_TYPE_ASSISTANT*/);
}