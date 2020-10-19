package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ExecutionImputationScopeFunction implements Serializable {

	private ScopeFunction holder;
	private ScopeFunction assistant;

	public ExecutionImputationScopeFunction setHolderFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			holder = null;
		else
			holder = EntityFinder.getInstance().find(ScopeFunction.class, identifier);
		return this;
	}
	
	public ExecutionImputationScopeFunction setAssistantFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			assistant = null;
		else
			assistant = EntityFinder.getInstance().find(ScopeFunction.class, identifier);
		return this;
	}
	
	@Override
	public String toString() {
		return holder+"/"+assistant;
	}
	
	public static final String FIELD_HOLDER = "holder";
	public static final String FIELD_ASSISTANT = "assistant";
}