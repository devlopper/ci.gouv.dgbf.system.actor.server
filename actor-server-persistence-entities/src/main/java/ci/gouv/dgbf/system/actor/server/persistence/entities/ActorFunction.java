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
@Entity @Table(name=ActorFunction.TABLE_NAME)
public class ActorFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTOR) @NotNull private Actor actor;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	
	@Override
	public ActorFunction setIdentifier(String identifier) {
		return (ActorFunction) super.setIdentifier(identifier);
	}
	
	public ActorFunction setActorFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setActor(null);
		else
			setActor(EntityFinder.getInstance().find(Actor.class, identifier));
		return this;
	}
	
	public ActorFunction setFunctionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setFunction(null);
		else
			setFunction(EntityFinder.getInstance().find(Function.class, identifier));
		return this;
	}
	
	public static final String FIELD_ACTOR = "actor";
	public static final String FIELD_FUNCTION = "function";
	
	public static final String TABLE_NAME = "FONCTION_ACCORDEE";
	
	public static final String COLUMN_ACTOR = "ACTEUR";
	public static final String COLUMN_FUNCTION = "FONCTION";	
}