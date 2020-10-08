package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.persistence.ManyToOne;
import javax.validation.constraints.NotNull;

@Embeddable
public class ExecutionImputationBudgetaryFunction implements Serializable {

	@ManyToOne @NotNull private BudgetaryFunction function;
	@ManyToOne private BudgetaryFunction assistant;

	public static final String FIELD_FUNCTION = "function";
	public static final String FIELD_ASSISTANT = "assistant";
}