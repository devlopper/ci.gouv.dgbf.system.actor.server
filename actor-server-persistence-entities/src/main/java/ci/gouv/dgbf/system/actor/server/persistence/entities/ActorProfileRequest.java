package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ActorProfileRequest.TABLE_NAME)
public class ActorProfileRequest extends AbstractActorRequest implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_PROFILE,nullable = false) @NotNull private Profile profile;
	
	/**/
	
	public static final String FIELD_PROFILE = "profile";
	public static final String FIELD_PROFILE_STRING = "profileAsString";
	
	public static final String COLUMN_PROFILE = "profile";
	
	public static final String TABLE_NAME = "TA_DEMANDE_PROFILE";	
}