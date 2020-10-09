package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class FunctionDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private FunctionTypeDto type;
	private String profileIdentifier;
	private Integer numberOfActorPerScope;
	private String profilesAsString;
	private String scopesAsString;
	private Integer numberOfScopes;
	private String scopeTypesAsString;
	private ArrayList<String> profilesAsStrings;
	private ArrayList<ScopeTypeDto> scopeTypes;
	private Boolean shared;
	private String sharedAsString;
	
	@Override
	public FunctionDto setIdentifier(String identifier) {
		return (FunctionDto) super.setIdentifier(identifier);
	}

}