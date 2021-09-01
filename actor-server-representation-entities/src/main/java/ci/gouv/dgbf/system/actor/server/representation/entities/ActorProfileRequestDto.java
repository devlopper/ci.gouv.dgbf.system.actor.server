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

	public static final String JSON_FIELD_PROFILE_AS_STRING = "profile";
	public static final String JSON_FIELD_PROFILE_TYPE_AS_STRING = "type_profile";
}