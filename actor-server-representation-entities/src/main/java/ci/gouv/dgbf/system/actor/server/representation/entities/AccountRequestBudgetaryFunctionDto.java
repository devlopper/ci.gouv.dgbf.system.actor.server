package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class AccountRequestBudgetaryFunctionDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private AccountRequestDto accountRequest;
	private BudgetaryFunctionDto budgetaryFunction;
	
	@Override
	public AccountRequestBudgetaryFunctionDto setIdentifier(String identifier) {
		return (AccountRequestBudgetaryFunctionDto) super.setIdentifier(identifier);
	}

}