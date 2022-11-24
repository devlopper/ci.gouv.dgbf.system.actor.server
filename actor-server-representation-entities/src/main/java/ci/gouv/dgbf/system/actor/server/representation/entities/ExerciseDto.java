package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ExerciseDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public ExerciseDto setIdentifier(String identifier) {
		return (ExerciseDto) super.setIdentifier(identifier);
	}

}