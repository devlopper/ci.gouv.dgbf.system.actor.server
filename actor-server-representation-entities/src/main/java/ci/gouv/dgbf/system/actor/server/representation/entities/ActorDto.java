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
	private String creationDateAsString;
	private String firstName;
	private String lastNames;
	private String names;
	private String electronicMailAddress;
	private String registrationNumber;
	private String postalBoxAddress;
	private String mobilePhoneNumber;
	private String officePhoneNumber;
	private String officePhoneExtension;
	private String username;
	private String password;
	private Byte notation;
	private String color; 
	private String administrativeFunction;
	private String sectionAsString;
	private String administrativeUnitAsString;
	private String civilityAsString;
	private String groupAsString;
	
	private AdministrativeUnitDto administrativeUnit;
	private CivilityDto civility;
	private IdentityGroupDto group;
	private Boolean emailSendableAfterCreation;
	
	private ArrayList<ProfileDto> profiles;
	private ArrayList<String> profilesCodes;
	private ArrayList<FunctionDto> functions;
	private ArrayList<String> functionsCodes;
	private ArrayList<PrivilegeDto> privileges;
	private ArrayList<PrivilegeDto> visibleModules;
	private ArrayList<ScopeDto> scopes;
	private ArrayList<ScopeDto> visibleSections;
	private ArrayList<String> sectionsIdentifiers;
	
	@Override
	public ActorDto setIdentifier(String identifier) {
		return (ActorDto) super.setIdentifier(identifier);
	}
}