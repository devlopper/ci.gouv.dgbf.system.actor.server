package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.List;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public abstract class AbstractActorRequestDto extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	protected ActorDto actor;
	protected String actorAsString;
	protected List<String> actorsIdentifiers;
	
	protected String comment;
	
	protected Boolean granted;
	protected String grantedAsString;
	
	protected String processingComment;
	
	protected Boolean ignoreExisting;
	
	/**/
	
	public static final String JSON_FIELD_IDENTIFIER = "identifiant";
	public static final String JSON_FIELD_ACTOR_AS_STRING = "acteur";
	public static final String JSON_FIELD_COMMENT = "commentaire";
	public static final String JSON_FIELD_GRANTED_AS_STRING = "accord";
	public static final String JSON_FIELD_PROCESSING_COMMENT = "commentaire_traitement";
}