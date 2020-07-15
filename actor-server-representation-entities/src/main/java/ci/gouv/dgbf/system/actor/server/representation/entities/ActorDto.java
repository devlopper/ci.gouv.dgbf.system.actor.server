package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActorDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	private IdentityDto identity;
	private String firstName;
	private String lastNames;
	private String names;
	private String electronicMailAddress;	
	private String username;
	private String password;
	
	private ArrayList<FunctionDto> functions;
	private ArrayList<PrivilegeDto> privileges;
	private ArrayList<ScopeDto> scopes;

	@Override
	public ActorDto setIdentifier(String identifier) {
		return (ActorDto) super.setIdentifier(identifier);
	}
}