package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorProfileRequestDto extends AbstractActorRequestDto implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ProfileDto profile;
	private String profileAsString;
	private String profileTypeAsString;
	private List<String> profilesIdentifiers;

}