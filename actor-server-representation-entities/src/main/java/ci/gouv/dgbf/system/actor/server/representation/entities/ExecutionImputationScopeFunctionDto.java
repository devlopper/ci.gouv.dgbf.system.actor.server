package ci.gouv.dgbf.system.actor.server.representation.entities;
import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter @NoArgsConstructor
public class ExecutionImputationScopeFunctionDto implements Serializable {

	private ScopeFunctionDto holder;
	private ScopeFunctionDto assistant;
	
	private Boolean holderOverridable;
	private Boolean assistantOverridable;
}