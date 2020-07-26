package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorFunctionDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ActorDto actor;
	private FunctionDto function;
	
	@Override
	public ActorFunctionDto setIdentifier(String identifier) {
		return (ActorFunctionDto) super.setIdentifier(identifier);
	}

}