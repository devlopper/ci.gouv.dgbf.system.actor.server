package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ScopeFunctionDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	private ScopeDto scope;
	private FunctionDto function;
	private String scopeAsString;
	private String functionAsString;
	private Integer numberOfActor;
	private Boolean shared;
	private String sharedAsString;
	private ArrayList<String> functionsIdentifiers;
	
	@Override
	public ScopeFunctionDto setIdentifier(String identifier) {
		return (ScopeFunctionDto) super.setIdentifier(identifier);
	}

}