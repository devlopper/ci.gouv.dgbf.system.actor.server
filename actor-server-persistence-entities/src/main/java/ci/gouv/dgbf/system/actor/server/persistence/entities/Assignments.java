package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.List;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Assignments.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = Assignments.FIELD___AUDIT_WHO__,column = @Column(name="AUDIT_ACTEUR"))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_WHAT__,column = @Column(name="AUDIT_ACTION"))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_WHEN__,column = @Column(name="AUDIT_DATE"))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name="AUDIT_FONCTIONALITE"))
})
public class Assignments extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {

	/* Imputation */
	
	@ManyToOne @JoinColumn(name=COLUMN_EXECUTION_IMPUTATION) @NotNull private ExecutionImputation executionImputation;
	
	@Transient private String sectionAsString;
	@Transient private String budgetSpecializationUnitAsString;
	@Transient private String actionAsString;
	@Transient private String activityAsString;
	@Transient private String economicNatureAsString;
	@Transient private String administrativeUnitAsString;
	@Transient private String activityCategoryAsString;
	@Transient private String expenditureNatureAsString;
	
	/* Affectations */
	
	/* Gestionnaire de crédits */
	
	@ManyToOne @JoinColumn(name=COLUMN_CREDIT_MANAGER_HOLDER) private ScopeFunction creditManagerHolder;
	@ManyToOne @JoinColumn(name=COLUMN_CREDIT_MANAGER_ASSISTANT) private ScopeFunction creditManagerAssistant;
	
	/* Ordonnateur */
	
	@ManyToOne @JoinColumn(name=COLUMN_AUTHORIZING_OFFICER_HOLDER) private ScopeFunction authorizingOfficerHolder;
	@ManyToOne @JoinColumn(name=COLUMN_AUTHORIZING_OFFICER_ASSISTANT) private ScopeFunction authorizingOfficerAssistant;
	
	/* Controleur financier */
	
	@ManyToOne @JoinColumn(name=COLUMN_FINANCIAL_CONTROLLER_HOLDER) private ScopeFunction financialControllerHolder;
	@ManyToOne @JoinColumn(name=COLUMN_FINANCIAL_CONTROLLER_ASSISTANT) private ScopeFunction financialControllerAssistant;
	
	/* Comptable */
	
	@ManyToOne @JoinColumn(name=COLUMN_ACCOUNTING_HOLDER) private ScopeFunction accountingHolder;
	@ManyToOne @JoinColumn(name=COLUMN_ACCOUNTING_ASSISTANT) private ScopeFunction accountingAssistant;
	
	/**/
	
	@Transient private String creditManagerHolderAsString;
	@Transient private String creditManagerAssistantAsString;	
	@Transient private String authorizingOfficerHolderAsString;
	@Transient private String authorizingOfficerAssistantAsString;	
	@Transient private String financialControllerHolderAsString;
	@Transient private String financialControllerAssistantAsString;	
	@Transient private String accountingHolderAsString;
	@Transient private String accountingAssistantAsString;
	
	/**/
	
	public Assignments(String identifier,String executionImputationIdentifier
			,String creditManagerHolderIdentifier,String creditManagerAssistantIdentifier
			,String authorizingOfficerHolder,String authorizingOfficerAssistant
			,String financialControllerHolder,String financialControllerAssistant
			,String accountingHolder,String accountingAssistant
			) {
		this.identifier = identifier;
		this.executionImputation = new ExecutionImputation().setIdentifier(executionImputationIdentifier);
		this.creditManagerHolder = new ScopeFunction().setIdentifier(creditManagerHolderIdentifier);
		this.creditManagerAssistant = new ScopeFunction().setIdentifier(creditManagerAssistantIdentifier);
		this.authorizingOfficerHolder = new ScopeFunction().setIdentifier(authorizingOfficerHolder);
		this.authorizingOfficerAssistant = new ScopeFunction().setIdentifier(authorizingOfficerAssistant);
		this.financialControllerHolder = new ScopeFunction().setIdentifier(financialControllerHolder);
		this.financialControllerAssistant = new ScopeFunction().setIdentifier(financialControllerAssistant);
		this.accountingHolder = new ScopeFunction().setIdentifier(accountingHolder);
		this.accountingAssistant = new ScopeFunction().setIdentifier(accountingAssistant);
	}
	
	@Override
	public Assignments setIdentifier(String identifier) {
		return (Assignments) super.setIdentifier(identifier);
	}
	
	/**/
	
	public static final String FIELD_EXECUTION_IMPUTATION = "executionImputation";
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING = "budgetSpecializationUnitAsString";
	public static final String FIELD_ACTION_AS_STRING = "actionAsString";
	public static final String FIELD_ACTIVITY_AS_STRING = "activityAsString";
	public static final String FIELD_ECONOMIC_NATURE_AS_STRING = "economicNatureAsString";
	public static final String FIELD_ADMINISTRATIVE_UNIT_AS_STRING = "administrativeUnitAsString";
	public static final String FIELD_ACTIVITY_CATEGORY_AS_STRING = "activityCategoryAsString";
	public static final String FIELD_EXPENDITURE_NATURE_AS_STRING = "expenditureNatureAsString";
	public static final String FIELD_CREDIT_MANAGER_HOLDER = "creditManagerHolder";
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT = "creditManagerAssistant";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER = "authorizingOfficerHolder";
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT = "authorizingOfficerAssistant";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER = "financialControllerHolder";
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT = "financialControllerAssistant";
	public static final String FIELD_ACCOUNTING_HOLDER = "accountingHolder";
	public static final String FIELD_ACCOUNTING_ASSISTANT = "accountingAssistant";
	
	public static final String FIELD_CREDIT_MANAGER_HOLDER_AS_STRING = "creditManagerHolderAsString";
	public static final String FIELD_CREDIT_MANAGER_ASSISTANT_AS_STRING = "creditManagerAssistantAsString";
	public static final String FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING = "authorizingOfficerHolderAsString";
	public static final String FIELD_AUTHORIZING_OFFICER_ASSISTANT_AS_STRING = "authorizingOfficerAssistantAsString";
	public static final String FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING = "financialControllerHolderAsString";
	public static final String FIELD_FINANCIAL_CONTROLLER_ASSISTANT_AS_STRING = "financialControllerAssistantAsString";
	public static final String FIELD_ACCOUNTING_HOLDER_AS_STRING = "accountingHolderAsString";
	public static final String FIELD_ACCOUNTING_ASSISTANT_AS_STRING = "accountingAssistantAsString";
	
	public static final List<String> FIELDS_SCOPES_FUNCTIONS = List.of(
			FIELD_CREDIT_MANAGER_HOLDER,FIELD_CREDIT_MANAGER_ASSISTANT
			,FIELD_AUTHORIZING_OFFICER_HOLDER,FIELD_AUTHORIZING_OFFICER_ASSISTANT
			,FIELD_FINANCIAL_CONTROLLER_HOLDER,FIELD_FINANCIAL_CONTROLLER_ASSISTANT
			,FIELD_ACCOUNTING_HOLDER,FIELD_ACCOUNTING_ASSISTANT
		);
	
	public static final String TABLE_NAME = "AFFECTATIONS";
	
	public static final String COLUMN_IDENTIFIER = "IDENTIFIANT";
	public static final String COLUMN_EXECUTION_IMPUTATION = "IMPUTATION";
	public static final String COLUMN_CREDIT_MANAGER_HOLDER = "GC";
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT = "A"+COLUMN_CREDIT_MANAGER_HOLDER;
	public static final String COLUMN_AUTHORIZING_OFFICER_HOLDER = "ORD";
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT = "A"+COLUMN_AUTHORIZING_OFFICER_HOLDER;
	public static final String COLUMN_FINANCIAL_CONTROLLER_HOLDER = "CF";
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT = "A"+COLUMN_FINANCIAL_CONTROLLER_HOLDER;
	public static final String COLUMN_ACCOUNTING_HOLDER = "CPT";
	public static final String COLUMN_ACCOUNTING_ASSISTANT = "A"+COLUMN_ACCOUNTING_HOLDER;
	
	public static final List<String> COLUMNS_SCOPES_FUNCTIONS = List.of(
			COLUMN_CREDIT_MANAGER_HOLDER,COLUMN_CREDIT_MANAGER_ASSISTANT
			,COLUMN_AUTHORIZING_OFFICER_HOLDER,COLUMN_AUTHORIZING_OFFICER_ASSISTANT
			,COLUMN_FINANCIAL_CONTROLLER_HOLDER,COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
			,COLUMN_ACCOUNTING_HOLDER,COLUMN_ACCOUNTING_ASSISTANT
		);
}