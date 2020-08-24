package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ActorProfile.TABLE_NAME)
public class ActorProfile extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTOR) @NotNull private Actor actor;
	@ManyToOne @JoinColumn(name = COLUMN_PROFILE) @NotNull private Profile profile;
	
	@Override
	public ActorProfile setIdentifier(String identifier) {
		return (ActorProfile) super.setIdentifier(identifier);
	}
	
	public ActorProfile setProfileFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setProfile(null);
		else
			setProfile(EntityFinder.getInstance().find(Profile.class, identifier));
		return this;
	}
	
	public ActorProfile setActorFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setActor(null);
		else
			setActor(EntityFinder.getInstance().find(Actor.class, identifier));
		return this;
	}
	
	@Override
	public String toString() {
		return actor+" : "+profile;
	}
	
	public static final String FIELD_PROFILE = "profile";
	public static final String FIELD_ACTOR = "actor";
	
	public static final String COLUMN_PROFILE = "profile";
	public static final String COLUMN_ACTOR = "acteur";
	
	public static final String TABLE_NAME = "ACTEUR_PROFILE";	
}