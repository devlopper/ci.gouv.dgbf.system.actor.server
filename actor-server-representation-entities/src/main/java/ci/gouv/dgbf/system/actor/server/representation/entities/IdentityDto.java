package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class IdentityDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	private String firstName;
	private String lastNames;
	private String names;
	private String electronicMailAddress;	
	
	private String registrationNumber;
	private String postalBox;
	private String mobilePhoneNumber;
	private String officePhoneNumber;
	private String officePhoneExtension;
	private AdministrativeUnitDto administrativeUnit;
	private String administrativeFunction;
	private CivilityDto civility;
	private IdentityGroupDto group;
	private String actOfAppointmentReference;
	private String actOfAppointmentSignatory;
	private String actOfAppointmentSignatureDateAsString;
	private Long actOfAppointmentSignatureTimestamp;
	
	@Override
	public IdentityDto setIdentifier(String identifier) {
		return (IdentityDto) super.setIdentifier(identifier);
	}
}