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
	private String scopeTypeAsString;
	private List<String> scopesIdentifiers;
	
	public static final String JSON_FIELD_SCOPE_AS_STRING = "domaine";
	public static final String JSON_FIELD_SCOPE_TYPE_AS_STRING = "type_domaine";
}