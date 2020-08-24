package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ProfileFunctionDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ProfileDto profile;
	private FunctionDto function;
	private String actorIdentifier;

	@Override
	public ProfileFunctionDto set__deletable__(Boolean __deletable__) {
		return (ProfileFunctionDto) super.set__deletable__(__deletable__);
	}
}