package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.field.FieldHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ExecutionImputation.TABLE_NAME)
public class ExecutionImputation extends AbstractImputation implements Serializable {
	private static final long serialVersionUID = 1L;
	
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
	
	public ExecutionImputation setCreditManagerAssistantFromIdentifier(String identifier) {
		getCreditManager(Boolean.TRUE).setAssistantFromIdentifier(identifier);
		return this;
	}
	
	public ExecutionImputationScopeFunction getAuthorizingOfficer(Boolean instantiateIfNull) {
		if(authorizingOfficer == null && Boolean.TRUE.equals(instantiateIfNull))
			authorizingOfficer = new ExecutionImputationScopeFunction();
		return authorizingOfficer;
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
	
	public static final String FIELD_CREDIT_MANAGER = "creditManager";
	public static final String FIELD_CREDIT_MANAGER_HOLDER = FieldHelper.join(FIELD_CREDIT_MANAGER,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT = FieldHelper.join(FIELD_CREDIT_MANAGER,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	
	public static final String FIELD_AUTHORIZING_OFFICER = "authorizingOfficer";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER = FieldHelper.join(FIELD_AUTHORIZING_OFFICER,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT = FieldHelper.join(FIELD_AUTHORIZING_OFFICER,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	
	public static final String FIELD_FINANCIAL_CONTROLLER = "financialController";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER = FieldHelper.join(FIELD_FINANCIAL_CONTROLLER,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT = FieldHelper.join(FIELD_FINANCIAL_CONTROLLER,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	
	public static final String FIELD_ACCOUNTING = "accounting";
	public static final String FIELD_ACCOUNTING_HOLDER = FieldHelper.join(FIELD_ACCOUNTING,ExecutionImputationScopeFunction.FIELD_HOLDER);
	public static final String FIELD_ACCOUNTING_ASSISTANT = FieldHelper.join(FIELD_ACCOUNTING,ExecutionImputationScopeFunction.FIELD_ASSISTANT);
	
	public static final String TABLE_NAME = "VM_APP_EX_IMPUTATION";
}