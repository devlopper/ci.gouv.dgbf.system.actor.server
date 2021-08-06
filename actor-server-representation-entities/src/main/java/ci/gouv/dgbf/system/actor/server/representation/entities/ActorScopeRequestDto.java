package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.List;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorScopeRequestDto extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ActorDto actor;
	private String actorAsString;
	private List<String> actorsIdentifiers;
	
	private ScopeDto scope;
	private String scopeAsString;
	private List<String> scopesIdentifiers;
	
	private Boolean granted;
	private String grantedAsString;

	private Boolean ignoreExisting;
}