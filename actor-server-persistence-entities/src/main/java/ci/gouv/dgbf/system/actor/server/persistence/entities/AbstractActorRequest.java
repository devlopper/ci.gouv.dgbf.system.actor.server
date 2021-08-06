package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringAuditedImpl;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.hibernate.envers.AuditOverride;
import org.hibernate.envers.AuditOverrides;
import org.hibernate.envers.Audited;
import org.hibernate.envers.RelationTargetAuditMode;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
@MappedSuperclass
@AuditOverrides(value = {
		@AuditOverride(forClass = AbstractIdentifiableSystemScalarStringAuditedImpl.class)
		,@AuditOverride(forClass = AbstractIdentifiableSystemScalarStringImpl.class)
	})
public abstract class AbstractActorRequest extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTOR,nullable = false) @NotNull
	@Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
	protected Actor actor;
	@Transient protected String actorAsString;
	
	@Column(name = COLUMN_GRANTED) protected Boolean granted;
	@Transient protected String grantedAsString;
	
	@Column(name = COLUMN_COMMENT) protected String comment;
	
	@Column(name = COLUMN_PROCESSING_COMMENT) protected String processingComment;
	
	/**/
	
	public static final String FIELD_ACTOR = "actor";
	public static final String FIELD_ACTOR_STRING = "actorAsString";
	public static final String FIELD_GRANTED = "granted";
	public static final String FIELD_GRANTED_AS_STRING = "grantedAsString";
	public static final String FIELDS_GRANTED_AND_GRANTED_AS_STRING = "grantedAndGrantedAsString";
	public static final String FIELD_COMMENT = "comment";
	public static final String FIELD_PROCESSING_COMMENT = "processingComment";
	
	public static final String COLUMN_ACTOR = "acteur";
	public static final String COLUMN_GRANTED = "accordee";
	public static final String COLUMN_COMMENT = "commentaire";	
	public static final String COLUMN_PROCESSING_COMMENT = "commentaire_traitement";
}