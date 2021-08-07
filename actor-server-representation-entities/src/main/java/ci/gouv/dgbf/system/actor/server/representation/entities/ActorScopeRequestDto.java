package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorScopeRequestDto extends AbstractActorRequestDto implements Serializable {
	private static final long serialVersionUID = 1L;

	private ScopeDto scope;
	private String scopeAsString;
	private List<String> scopesIdentifiers;
}