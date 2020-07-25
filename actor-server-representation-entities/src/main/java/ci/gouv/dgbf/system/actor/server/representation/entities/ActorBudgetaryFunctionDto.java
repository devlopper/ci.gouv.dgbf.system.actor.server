package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorBudgetaryFunctionDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ActorDto actor;
	private BudgetaryFunctionDto budgetaryFunction;
	
	@Override
	public ActorBudgetaryFunctionDto setIdentifier(String identifier) {
		return (ActorBudgetaryFunctionDto) super.setIdentifier(identifier);
	}

}