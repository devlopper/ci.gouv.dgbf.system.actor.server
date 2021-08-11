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
@Entity @Table(name=ActorScopeRequest.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = ActorScopeRequest.FIELD___AUDIT_WHO__,column = @Column(name=ActorScopeRequest.COLUMN___AUDIT_WHO__))
		,@AttributeOverride(name = ActorScopeRequest.FIELD___AUDIT_WHAT__,column = @Column(name=ActorScopeRequest.COLUMN___AUDIT_WHAT__))
		,@AttributeOverride(name = ActorScopeRequest.FIELD___AUDIT_WHEN__,column = @Column(name=ActorScopeRequest.COLUMN___AUDIT_WHEN__))
		,@AttributeOverride(name = ActorScopeRequest.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name=ActorScopeRequest.COLUMN___AUDIT_FUNCTIONALITY__))
})
public class ActorScopeRequest extends AbstractActorRequest implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE,nullable = false) @NotNull
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	private Scope scope;
	@Transient private String scopeAsString;
	@Transient private String scopeTypeAsString;
	
	@Override
	public ActorScopeRequest setActor(Actor actor) {
		return (ActorScopeRequest) super.setActor(actor);
	}
	
	@Override
	public String toString() {
		return String.format(TO_STRING_FORMAT, actor,scope,granted);
	}
	
	private static final String TO_STRING_FORMAT = "%s|%s|%s";
	
	/**/
	
	public static final String FIELD_SCOPE = "scope";
	public static final String FIELD_SCOPE_STRING = "scopeAsString";
	public static final String FIELDS_ACTOR_AS_STRING_SCOPE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING = "actorAsStringScopeAsStringGrantedAndGrantedAsString";
	public static final String FIELDS_ACTOR_AS_STRING_SCOPE_TYPE_AS_STRING_SCOPE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING = "actorAsStringScopeTypeAsStringScopeAsStringGrantedAndGrantedAsString";
	
	public static final String TABLE_NAME = "TA_DEMANDE_DOMAINE";	
	
	public static final String COLUMN_SCOPE = "DOMAINE";
	public static final String COLUMN___AUDIT_WHO__ = "AUDIT_ACTEUR";
	public static final String COLUMN___AUDIT_FUNCTIONALITY__ = "AUDIT_FONCTIONNALITE";
	public static final String COLUMN___AUDIT_WHAT__ = "AUDIT_ACTION";
	public static final String COLUMN___AUDIT_WHEN__ = "AUDIT_DATE";
}