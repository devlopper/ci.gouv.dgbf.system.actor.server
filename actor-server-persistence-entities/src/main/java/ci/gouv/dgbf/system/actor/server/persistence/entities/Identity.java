package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Identity.TABLE_NAME)
public class Identity extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_FIRST_NAME) @NotNull private String firstName;
	@Column(name = COLUMN_LAST_NAMES) @NotNull private String lastNames;
	@Column(name = COLUMN_ELECTRONIC_MAIL_ADDRESS) @NotNull private String electronicMailAddress;
	
	@Column(name = COLUMN_REGISTRATION_NUMBER) private String registrationNumber;
	@Column(name = COLUMN_POSTAL_BOX) private String postalBox;
	@Column(name = COLUMN_MOBILE_PHONE_NUMBER) private String mobilePhoneNumber;
	@Column(name = COLUMN_OFFICE_PHONE_NUMBER) private String officePhoneNumber;
	@Column(name = COLUMN_OFFICE_PHONE_EXTENSION) private String officePhoneExtension;
	@ManyToOne @JoinColumn(name = COLUMN_ADMINISTRATIVE_UNIT) private AdministrativeUnit administrativeUnit;
	@Column(name = COLUMN_ADMINISTRATIVE_FUNCTION) private String administrativeFunction;
	@ManyToOne @JoinColumn(name = COLUMN_CIVILITY) private Civility civility;
	@ManyToOne @JoinColumn(name = COLUMN_GROUP) private IdentityGroup group;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_REFERENCE) private String actOfAppointmentReference;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_SIGNATORY) private String actOfAppointmentSignatory;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_SIGNATURE_DATE) private LocalDate actOfAppointmentSignatureDate;
	
	@Transient private String names;
	@Transient private Long actOfAppointmentSignatureTimestamp;
	
	@Override
	public Identity setIdentifier(String identifier) {
		return (Identity) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_REGISTRATION_NUMBER = "registrationNumber";
	public static final String FIELD_POSTAL_BOX = "postalBox";
	public static final String FIELD_MOBILE_PHONE_NUMBER = "mobilePhoneNumber";
	public static final String FIELD_OFFICE_PHONE_NUMBER = "officePhoneNumber";
	public static final String FIELD_OFFICE_PHONE_EXTENSION = "officePhoneExtension";
	public static final String FIELD_ADMINISTRATIVE_UNIT = "administrativeUnit";
	public static final String FIELD_ADMINISTRATIVE_FUNCTION = "administrativeFunction";
	public static final String FIELD_CIVILITY = "civility";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_ACT_OF_APPOINTMENT_REFERENCE = "actOfAppointmentReference";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATORY = "actOfAppointmentSignatory";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "actOfAppointmentSignatureDate";
	
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_TIMESTAMP = "actOfAppointmentSignatureTimestamp";
	public static final String FIELD_NAMES = "names";
	
	public static final String TABLE_NAME = "IDENTITE";
	
	public static final String COLUMN_FIRST_NAME = "NOM";
	public static final String COLUMN_LAST_NAMES = "PRENOMS";
	public static final String COLUMN_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
	public static final String COLUMN_REGISTRATION_NUMBER = "MATRICULE";
	public static final String COLUMN_POSTAL_BOX = "BOITE_POSTALE";
	public static final String COLUMN_MOBILE_PHONE_NUMBER = "NUMERO_MOBILE";
	public static final String COLUMN_OFFICE_PHONE_NUMBER = "NUMERO_BUREAU";
	public static final String COLUMN_OFFICE_PHONE_EXTENSION = "POSTE_BUREAU";
	public static final String COLUMN_ADMINISTRATIVE_UNIT = "UNITE_ADMINISTRATIVE";
	public static final String COLUMN_ADMINISTRATIVE_FUNCTION = "FONCTION_ADMINISTRATIVE";
	public static final String COLUMN_CIVILITY = "CIVILITE";
	public static final String COLUMN_GROUP = "GROUPE";
	public static final String COLUMN_ACT_OF_APPOINTMENT_REFERENCE = "REFERENCE_ACTE_NOMINATION";
	public static final String COLUMN_ACT_OF_APPOINTMENT_SIGNATORY = "SIGNATAIRE_ACTE_NOMINATION";
	public static final String COLUMN_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "DATE_SIGNATURE_ACTE_NOMINATION";
	
	/**/
	
	public static interface Interface {
		String getFirstName();
		String getLastNames();
		String getElectronicMailAddress();
	}
	
	@Override
	public String toString() {
		return firstName+" "+lastNames+" "+electronicMailAddress;
	}
}