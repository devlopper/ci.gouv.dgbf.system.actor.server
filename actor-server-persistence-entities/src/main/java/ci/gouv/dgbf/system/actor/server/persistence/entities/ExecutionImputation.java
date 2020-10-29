package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SecondaryTable;
import javax.persistence.SecondaryTables;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ExecutionImputation.TABLE_NAME)
@SecondaryTables(value= @SecondaryTable(name = ExecutionImputation.VIEW_NAME))
public class ExecutionImputation extends AbstractImputation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(table = VIEW_NAME,name = COLUMN_CREDIT_MANAGER_HOLDER_IDENTIFIER)
	private String creditManagerHolderIdentifier;
	@Column(table = VIEW_NAME,name = COLUMN_CREDIT_MANAGER_HOLDER_CODE_NAME)
	private String creditManagerHolderCodeName;
	//private String creditManagerAssistantCodeName;
	
	@Column(table = VIEW_NAME,name = COLUMN_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER)
	private String authorizingOfficerHolderIdentifier;
	@Column(table = VIEW_NAME,name = COLUMN_AUTHORIZING_OFFICER_HOLDER_CODE_NAME)
	private String authorizingOfficerHolderCodeName;
	//private String assistantCodeName;
	
	@Column(table = VIEW_NAME,name = COLUMN_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER)
	private String financialControllerHolderIdentifier;
	@Column(table = VIEW_NAME,name = COLUMN_FINANCIAL_CONTROLLER_HOLDER_CODE_NAME)
	private String financialControllerHolderCodeName;
	//private String assistantCodeName;
	
	@Column(table = VIEW_NAME,name = COLUMN_ACCOUNTING_HOLDER_IDENTIFIER)
	private String accountingHolderIdentifier;
	@Column(table = VIEW_NAME,name = COLUMN_ACCOUNTING_HOLDER_CODE_NAME)
	private String accountingHolderCodeName;
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
	
	public ExecutionImputation setCreditManagerHolderFromIdentifier(String identifier) {
		getCreditManager(Boolean.TRUE).setHolderFromIdentifier(identifier);
		return this;
	}
	
	public ScopeFunction getCreditManagerHolder() {
		return creditManager == null ? null : creditManager.getHolder();
	}
	
	public ExecutionImputation setCreditManagerAssistantFromIdentifier(String identifier) {
		getCreditManager(Boolean.TRUE).setAssistantFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputationScopeFunction getAuthorizingOfficer(Boolean instantiateIfNull) {
		if(authorizingOfficer == null && Boolean.TRUE.equals(instantiateIfNull))
			authorizingOfficer = new ExecutionImputationScopeFunction();
		return authorizingOfficer;
	}
	
	public ScopeFunction getAuthorizingOfficerHolder() {
		return authorizingOfficer == null ? null : authorizingOfficer.getHolder();
	}
	
	public ExecutionImputation setAuthorizingOfficerHolderFromIdentifier(String identifier) {
		getAuthorizingOfficer(Boolean.TRUE).setHolderFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setAuthorizingOfficerAssistantFromIdentifier(String identifier) {
		getAuthorizingOfficer(Boolean.TRUE).setAssistantFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputationScopeFunction getFinancialController(Boolean instantiateIfNull) {
		if(financialController == null && Boolean.TRUE.equals(instantiateIfNull))
			financialController = new ExecutionImputationScopeFunction();
		return financialController;
	}
	
	public ScopeFunction getFinancialControllerHolder() {
		return financialController == null ? null : financialController.getHolder();
	}
	
	public ExecutionImputation setFinancialControllerHolderFromIdentifier(String identifier) {
		getFinancialController(Boolean.TRUE).setHolderFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setFinancialControllerAssistantFromIdentifier(String identifier) {
		getFinancialController(Boolean.TRUE).setAssistantFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputationScopeFunction getAccounting(Boolean instantiateIfNull) {
		if(accounting == null && Boolean.TRUE.equals(instantiateIfNull))
			accounting = new ExecutionImputationScopeFunction();
		return accounting;
	}
	
	public ScopeFunction getAccountingHolder() {
		return accounting == null ? null : accounting.getHolder();
	}
	
	public ExecutionImputation setAccountingHolderFromIdentifier(String identifier) {
		getAccounting(Boolean.TRUE).setHolderFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputation setAccountingAssistantFromIdentifier(String identifier) {
		getAccounting(Boolean.TRUE).setAssistantFromIdentifier(identifier);
		return this;
	}
	
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
	
	public Boolean hasScopeFunction(ScopeFunction scopeFunction) {
		if(scopeFunction == null || StringHelper.isBlank(scopeFunction.getIdentifier()))
			return Boolean.FALSE;
		if(scopeFunction.getIdentifier().equals(creditManagerHolderIdentifier) || scopeFunction.getIdentifier().equals(authorizingOfficerHolderIdentifier)
				 || scopeFunction.getIdentifier().equals(financialControllerHolderIdentifier) || scopeFunction.getIdentifier().equals(accountingHolderIdentifier))
			return Boolean.TRUE;
		return Boolean.FALSE;
	}
	
	public static final String FIELD_CREDIT_MANAGER = "creditManager";
	public static final String FIELD_CREDIT_MANAGER_HOLDER = FieldHelper.join(FIELD_CREDIT_MANAGER,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT = FieldHelper.join(FIELD_CREDIT_MANAGER,ExecutionImputationScopeFunction.FIELD_ASSISTANT);	
	public static final String FIELD_CREDIT_MANAGER_HOLDER_IDENTIFIER = "creditManagerHolderIdentifier";
	public static final String FIELD_CREDIT_MANAGER_HOLDER_CODE_NAME = "creditManagerHolderCodeName";
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT_CODE_NAME = "creditManagerAssistantCodeName";
	
	public static final String FIELD_AUTHORIZING_OFFICER = "authorizingOfficer";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER = FieldHelper.join(FIELD_AUTHORIZING_OFFICER,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT = FieldHelper.join(FIELD_AUTHORIZING_OFFICER,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER = "authorizingOfficerHolderIdentifier";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_CODE_NAME = "authorizingOfficerHolderCodeName";
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT_CODE_NAME = "authorizingOfficerAssistantCodeName";
	
	public static final String FIELD_FINANCIAL_CONTROLLER = "financialController";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER = FieldHelper.join(FIELD_FINANCIAL_CONTROLLER,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT = FieldHelper.join(FIELD_FINANCIAL_CONTROLLER,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER = "financialControllerHolderIdentifier";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_CODE_NAME = "financialControllerHolderCodeName";
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT_CODE_NAME = "financialControllerAssistantCodeName";
	
	public static final String FIELD_ACCOUNTING = "accounting";
	public static final String FIELD_ACCOUNTING_HOLDER = FieldHelper.join(FIELD_ACCOUNTING,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_ACCOUNTING_ASSISTANT = FieldHelper.join(FIELD_ACCOUNTING,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	public static final String FIELD_ACCOUNTING_HOLDER_IDENTIFIER = "accountingHolderIdentifier";
	public static final String FIELD_ACCOUNTING_HOLDER_CODE_NAME = "accountingHolderCodeName";
	public static final String FIELD_ACCOUNTING_ASSISTANT_CODE_NAME = "accountingAssistantCodeName";
	
	public static final String TABLE_NAME = "VM_APP_EX_IMPUTATION";
	public static final String VIEW_NAME = "V_APP_EX_IMPUTATION_POSTE";
	
	public static final String COLUMN_CREDIT_MANAGER_HOLDER_IDENTIFIER = "GC_IDENTIFIANT";
	public static final String COLUMN_CREDIT_MANAGER_HOLDER_CODE_NAME = "GC_CODE_LIBELLE";
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER = "ORD_IDENTIFIANT";
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER_CODE_NAME = "ORD_CODE_LIBELLE";
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER = "CF_IDENTIFIANT";
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER_CODE_NAME = "CF_CODE_LIBELLE";
	public static final String COLUMN_ACCOUNTING_HOLDER_IDENTIFIER = "CPT_IDENTIFIANT";
	public static final String COLUMN_ACCOUNTING_HOLDER_CODE_NAME = "CPT_CODE_LIBELLE";
}