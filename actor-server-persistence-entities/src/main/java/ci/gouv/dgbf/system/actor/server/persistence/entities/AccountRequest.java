package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=AccountRequest.TABLE_NAME)
public class AccountRequest extends AbstractIdentifiableSystemScalarStringImpl implements Identity.Interface,Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_IDENTITY) @NotNull private Identity identity;
	@Column(name = COLUMN_CREATION_DATE) @NotNull private LocalDateTime creationDate;
	@Column(name = COLUMN_ACCESS_TOKEN) @NotNull private String accessToken;
	@Column(name = COLUMN_SUBMISSION_DATE) private LocalDateTime submissionDate;
	
	@Transient private String firstName;
	@Transient private String lastNames;
	@Transient private String names;
	@Transient private String electronicMailAddress;	
	@Transient private String registrationNumber;
	@Transient private String postalBoxAddress;
	@Transient private String mobilePhoneNumber;
	@Transient private String officePhoneNumber;
	@Transient private String officePhoneExtension;
	@Transient private AdministrativeUnit administrativeUnit;
	@Transient private String administrativeUnitAsString;
	@Transient private String administrativeFunction;
	@Transient private String sectionAsString;
	@Transient private Civility civility;
	@Transient private String civilityAsString;
	@Transient private IdentityGroup group;
	@Transient private String groupAsString;
	@Transient private String actOfAppointmentReference;
	@Transient private String actOfAppointmentSignatory;
	@Transient private LocalDate actOfAppointmentSignatureDate;	
	@Transient private Long actOfAppointmentSignatureDateAsTimestamp;
	@Transient private String actOfAppointmentSignatureDateAsString;
	@Transient private String creationDateAsString;
	@Transient private Long creationDateAsTimestamp;
	@Transient private String submissionDateAsString;
	@Transient private Long submissionDateAsTimestamp;
	@Transient private String rejectReason;
	
	@Transient private Collection<BudgetaryFunction> budgetaryFunctions;
	@Transient private Collection<Function> functions;
	
	@Override
	public AccountRequest setIdentifier(String identifier) {
		return (AccountRequest) super.setIdentifier(identifier);
	}
	
	public AccountRequest setIdentityFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setIdentity(null);
		else
			setIdentity(EntityFinder.getInstance().find(Identity.class, identifier));
		return this;
	}
	
	public static final String FIELD_IDENTITY = "identity";
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_REGISTRATION_NUMBER = "registrationNumber";
	public static final String FIELD_POSTAL_BOX_ADDRESS = "postalBoxAddress";
	public static final String FIELD_MOBILE_PHONE_NUMBER = "mobilePhoneNumber";
	public static final String FIELD_OFFICE_PHONE_NUMBER = "officePhoneNumber";
	public static final String FIELD_OFFICE_PHONE_EXTENSION = "officePhoneExtension";
	public static final String FIELD_ADMINISTRATIVE_UNIT = "administrativeUnit";
	public static final String FIELD_ADMINISTRATIVE_UNIT_AS_STRING = "administrativeUnitAsString";
	public static final String FIELD_ADMINISTRATIVE_FUNCTION = "administrativeFunction";
	public static final String FIELD_CIVILITY = "civility";
	public static final String FIELD_CIVILITY_AS_STRING = "civilityAsString";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_GROUP_AS_STRING = "groupAsString";
	public static final String FIELD_ACT_OF_APPOINTMENT_REFERENCE = "actOfAppointmentReference";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATORY = "actOfAppointmentSignatory";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "actOfAppointmentSignatureDate";	
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_STRING = "actOfAppointmentSignatureDateAsString";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_TIMESTAMP = "actOfAppointmentSignatureDateAsTimestamp";
	public static final String FIELD_NAMES = "names";
	public static final String FIELD_CREATION_DATE = "creationDate";
	public static final String FIELD_CREATION_DATE_AS_STRING = "creationDateAsString";
	public static final String FIELD_CREATION_DATE_AS_TIMESTAMP = "creationDateAsTimestamp";
	public static final String FIELD_ACCESS_TOKEN = "accessToken";
	public static final String FIELD_SUBMISSION_DATE = "submissionDate";
	public static final String FIELD_SUBMISSION_DATE_AS_STRING = "submissionDateAsString";
	public static final String FIELD_SUBMISSION_DATE_AS_TIMESTAMP = "submissionDateAsTimestamp";
	public static final String FIELD_FUNCTIONS = "functions";
	public static final String FIELD_BUDGETARY_FUNCTIONS = "budgetaryFunctions";
	
	public static final String TABLE_NAME = "DEMANDE_COMPTE";
	
	public static final String COLUMN_IDENTITY = "IDENTITE";
	public static final String COLUMN_CREATION_DATE = "DATE_CREATION";
	public static final String COLUMN_ACCESS_TOKEN = "JETON_ACCES";
	public static final String COLUMN_SUBMISSION_DATE = "DATE_SOUMISSION";
}