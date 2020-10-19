package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.AssociationOverride;
import javax.persistence.AssociationOverrides;
import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
//@Entity @Table(name=ExecutableImputation.TABLE_NAME)
@AttributeOverrides(value = {
		@AttributeOverride(name=ExecutableImputation.FIELD_IDENTIFIER,column = @Column(name="LDEP_ID"))
	})
public class ExecutableImputation extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {

	@ManyToOne @JoinColumn(name=COLUMN_EXECUTION_IMPUTATION) @NotNull private ExecutionImputation executionImputation;
	
	@Embedded @AssociationOverrides(value = {
			@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_FUNCTION,joinColumns =  @JoinColumn(name=COLUMN_CREDIT_MANAGER_FUNCTION))
			,@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_ASSISTANT,joinColumns = @JoinColumn(name=COLUMN_CREDIT_MANAGER_ASSISTANT))
		})
	private ExecutionImputationBudgetaryFunction creditManager = new ExecutionImputationBudgetaryFunction();
	
	@Embedded @AssociationOverrides(value = {
			@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_FUNCTION,joinColumns =  @JoinColumn(name=COLUMN_AUTHORIZING_OFFICER_FUNCTION))
			,@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_ASSISTANT,joinColumns = @JoinColumn(name=COLUMN_AUTHORIZING_OFFICER_ASSISTANT))
		})
	private ExecutionImputationBudgetaryFunction authorizingOfficer = new ExecutionImputationBudgetaryFunction();
	
	@Embedded @AssociationOverrides(value = {
			@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_FUNCTION,joinColumns =  @JoinColumn(name=COLUMN_FINANCIAL_CONTROLLER_FUNCTION))
			,@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_ASSISTANT,joinColumns = @JoinColumn(name=COLUMN_FINANCIAL_CONTROLLER_ASSISTANT))
		})
	private ExecutionImputationBudgetaryFunction financialController = new ExecutionImputationBudgetaryFunction();
	
	@Embedded @AssociationOverrides(value = {
			@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_FUNCTION,joinColumns =  @JoinColumn(name=COLUMN_ACCOUNTING_FUNCTION))
			,@AssociationOverride(name=ExecutionImputationBudgetaryFunction.FIELD_ASSISTANT,joinColumns = @JoinColumn(name=COLUMN_ACCOUNTING_ASSISTANT))
		})
	private ExecutionImputationBudgetaryFunction accounting = new ExecutionImputationBudgetaryFunction();
	
	public static final String FIELD_EXECUTION_IMPUTATION = "executionImputation";
	public static final String FIELD_CREDIT_MANAGER = "creditManager";
	public static final String FIELD_AUTHORIZING_OFFICER = "authorizingOfficer";
	public static final String FIELD_FINANCIAL_CONTROLLER = "financialController";
	public static final String FIELD_ACCOUNTING = "accounting";
	
	public static final String TABLE_NAME = "IMPUTATION_EXECUTABLE";
	
	public static final String COLUMN_PREFIX = "LDEP_FCT_";
	
	public static final String COLUMN_EXECUTION_IMPUTATION = "LIGNE_DEPENSE";
	public static final String COLUMN_CREDIT_MANAGER_FUNCTION = COLUMN_PREFIX+"GC";
	public static final String COLUMN_CREDIT_MANAGER_ASSISTANT = COLUMN_PREFIX+"AGC";
	public static final String COLUMN_AUTHORIZING_OFFICER_FUNCTION = COLUMN_PREFIX+"ORD";
	public static final String COLUMN_AUTHORIZING_OFFICER_ASSISTANT = COLUMN_PREFIX+"AORD";
	public static final String COLUMN_FINANCIAL_CONTROLLER_FUNCTION = COLUMN_PREFIX+"CF";
	public static final String COLUMN_FINANCIAL_CONTROLLER_ASSISTANT = COLUMN_PREFIX+"ACF";
	public static final String COLUMN_ACCOUNTING_FUNCTION = COLUMN_PREFIX+"CPT";
	public static final String COLUMN_ACCOUNTING_ASSISTANT = COLUMN_PREFIX+"ACPT";
}