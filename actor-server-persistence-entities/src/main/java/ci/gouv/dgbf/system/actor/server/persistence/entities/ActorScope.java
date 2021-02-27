package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ActorScope.TABLE_NAME) @Access(AccessType.FIELD)
public class ActorScope extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTOR,nullable = false) @NotNull private Actor actor;
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE,nullable = false) @NotNull private Scope scope;
	@Column(name = COLUMN_VISIBLE) private Boolean visible;
	
	@Transient private String actorAsString;
	@Transient private String scopeAsString;
	
	@Override
	public ActorScope setIdentifier(String identifier) {
		return (ActorScope) super.setIdentifier(identifier);
	}
	
	public ActorScope setScopeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setScope(null);
		else
			setScope(EntityFinder.getInstance().find(Scope.class, identifier));
		return this;
	}
	
	public ActorScope setActorFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setActor(null);
		else
			setActor(EntityFinder.getInstance().find(Actor.class, identifier));
		return this;
	}
	
	@Override
	public String toString() {
		return actor+"|"+scope+"|"+visible;
	}
	
	public static final String FIELD_ACTOR = "actor";
	public static final String FIELD_SCOPE = "scope";
	public static final String FIELD_VISIBLE = "visible";
	public static final String FIELD_ACTOR_STRING = "actorAsString";
	public static final String FIELD_SCOPE_STRING = "scopeAsString";
	
	public static final String COLUMN_ACTOR = "acteur";
	public static final String COLUMN_SCOPE = "domaine";
	public static final String COLUMN_VISIBLE = "visible";
	
	public static final String TABLE_NAME = "ACTEUR_DOMAINE";	
}