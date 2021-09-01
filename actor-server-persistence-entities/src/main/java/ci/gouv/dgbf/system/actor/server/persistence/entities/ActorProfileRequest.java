package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.hibernate.envers.Audited;
import org.hibernate.envers.RelationTargetAuditMode;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ActorProfileRequest.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = ActorProfileRequest.FIELD___AUDIT_WHO__,column = @Column(name=ActorProfileRequest.COLUMN___AUDIT_WHO__))
		,@AttributeOverride(name = ActorProfileRequest.FIELD___AUDIT_WHAT__,column = @Column(name=ActorProfileRequest.COLUMN___AUDIT_WHAT__))
		,@AttributeOverride(name = ActorProfileRequest.FIELD___AUDIT_WHEN__,column = @Column(name=ActorProfileRequest.COLUMN___AUDIT_WHEN__))
		,@AttributeOverride(name = ActorProfileRequest.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name=ActorProfileRequest.COLUMN___AUDIT_FUNCTIONALITY__))
})
public class ActorProfileRequest extends AbstractActorRequest implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_PROFILE,nullable = false) @NotNull
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	private Profile profile;
	@Transient private String profileAsString;
	@Transient private String profileTypeAsString;
	
	@Override
	public ActorProfileRequest setActor(Actor actor) {
		return (ActorProfileRequest) super.setActor(actor);
	}
	
	/**/
	
	public static final String FIELD_PROFILE = "profile";
	public static final String FIELD_PROFILE_AS_STRING = "profileAsString";
	public static final String FIELD_PROFILE_TYPE_AS_STRING = "profileTypeAsString";
	public static final String FIELDS_ACTOR_AS_STRING_PROFILE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING = "actorAsStringProfileAsStringGrantedAndGrantedAsString";
	public static final String FIELDS_ACTOR_AS_STRING_PROFILE_TYPE_AS_STRING_PROFILE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING = "actorAsStringProfileTypeAsStringProfileAsStringGrantedAndGrantedAsString";
	
	public static final String COLUMN_PROFILE = "PROFILE";
	public static final String COLUMN___AUDIT_WHO__ = "AUDIT_ACTEUR";
	public static final String COLUMN___AUDIT_FUNCTIONALITY__ = "AUDIT_FONCTIONNALITE";
	public static final String COLUMN___AUDIT_WHAT__ = "AUDIT_ACTION";
	public static final String COLUMN___AUDIT_WHEN__ = "AUDIT_DATE";
	
	public static final String LABEL = "Demande "+Profile.LABEL;
	
	public static final String TABLE_NAME = "TA_DEMANDE_PROFILE";	
}