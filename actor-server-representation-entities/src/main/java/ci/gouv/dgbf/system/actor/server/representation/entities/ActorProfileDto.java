package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorProfileDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ActorDto actor;
	private ProfileDto profile;
	
	@Override
	public ActorProfileDto setIdentifier(String identifier) {
		return (ActorProfileDto) super.setIdentifier(identifier);
	}

	@Override
	public ActorProfileDto set__deletable__(Boolean __deletable__) {
		return (ActorProfileDto) super.set__deletable__(__deletable__);
	}
}