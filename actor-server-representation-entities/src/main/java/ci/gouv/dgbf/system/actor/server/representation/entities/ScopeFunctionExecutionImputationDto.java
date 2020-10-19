package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ScopeFunctionExecutionImputationDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public ScopeFunctionExecutionImputationDto setIdentifier(String identifier) {
		return (ScopeFunctionExecutionImputationDto) super.setIdentifier(identifier);
	}

}