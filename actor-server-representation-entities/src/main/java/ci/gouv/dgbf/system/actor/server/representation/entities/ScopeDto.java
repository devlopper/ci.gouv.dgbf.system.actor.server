package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ScopeDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ScopeTypeDto type;
	private ActorDto actor;
	private String actorIdentifier;
	
	@Override
	public ScopeDto set__deletable__(Boolean __deletable__) {
		return (ScopeDto) super.set__deletable__(__deletable__);
	}
	
	@Override
	public ScopeDto setIdentifier(String identifier) {
		return (ScopeDto) super.setIdentifier(identifier);
	}
	
	@Override
	public String toString() {
		return code+" | "+name+" | "+type;
	}
}