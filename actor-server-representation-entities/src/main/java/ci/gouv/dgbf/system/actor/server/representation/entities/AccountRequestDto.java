package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class AccountRequestDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private IdentityDto identity;
	private String firstName;
	private String lastNames;
	private String names;
	private String electronicMailAddress;	
	private String registrationNumber;
	private String postalBoxAddress;
	private String mobilePhoneNumber;
	private String officePhoneNumber;
	private String officePhoneExtension;
	private AdministrativeUnitDto administrativeUnit;
	private String administrativeUnitAsString;
	private String administrativeFunction;
	private String sectionAsString;
	private CivilityDto civility;
	private String civilityAsString;
	private IdentityGroupDto group;
	private String groupAsString;
	private String actOfAppointmentReference;
	private String actOfAppointmentSignatory;
	private LocalDate actOfAppointmentSignatureDate;	
	private String actOfAppointmentSignatureDateAsString;
	private Long actOfAppointmentSignatureDateAsTimestamp;
	private String creationDateAsString;
	private Long creationDateAsTimestamp;
	private String submissionDateAsString;
	private Long submissionDateAsTimestamp;
	private String accessToken;
	private String rejectReason;
	
	private ArrayList<FunctionDto> functions;
	
	@Override
	public AccountRequestDto setIdentifier(String identifier) {
		return (AccountRequestDto) super.setIdentifier(identifier);
	}
	
	/**/
	
	@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
	public static class AccountRequestCreationDto {
		
		private String firstName;
		private String lastNames;
		private String electronicMailAddress;	
		private String registrationNumber;
		private String postalBoxAddress;
		private String mobilePhoneNumber;
		private String officePhoneNumber;
		private String officePhoneExtension;
		private String administrativeUnitIdentifier;
		private String administrativeFunction;
		private String civilityIdentfier;
		private String identityGroupIdentifier;
	}
}