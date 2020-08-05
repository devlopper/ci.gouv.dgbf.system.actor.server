package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=AccountRequestBudgetaryFunction.TABLE_NAME)
public class AccountRequestBudgetaryFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACCOUNT_REQUEST) @NotNull private AccountRequest accountRequest;
	@ManyToOne @JoinColumn(name = COLUMN_BUDGETARY_FUNCTION) @NotNull private BudgetaryFunction budgetaryFunction;
	
	@Override
	public AccountRequestBudgetaryFunction setIdentifier(String identifier) {
		return (AccountRequestBudgetaryFunction) super.setIdentifier(identifier);
	}
	
	public AccountRequestBudgetaryFunction setAccountRequestFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setAccountRequest(null);
		else
			setAccountRequest(EntityFinder.getInstance().find(AccountRequest.class, identifier));
		return this;
	}
	
	public AccountRequestBudgetaryFunction setBudgetaryFunctionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setBudgetaryFunction(null);
		else
			setBudgetaryFunction(EntityFinder.getInstance().find(BudgetaryFunction.class, identifier));
		return this;
	}
	
	public static final String FIELD_ACCOUNT_REQUEST = "accountRequest";
	public static final String FIELD_BUDGETARY_FUNCTION = "budgetaryFunction";
	
	public static final String TABLE_NAME = "FONCTION_BUDGETAIRE_DEMANDEE";
	
	public static final String COLUMN_ACCOUNT_REQUEST = "DEMANDE_COMPTE";
	public static final String COLUMN_BUDGETARY_FUNCTION = "FONCTION_BUDGETAIRE";
}