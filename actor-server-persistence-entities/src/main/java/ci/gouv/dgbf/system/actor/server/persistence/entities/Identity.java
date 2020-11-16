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

import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.hibernate.envers.Audited;
import org.hibernate.envers.RelationTargetAuditMode;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Identity.TABLE_NAME)
//@org.hibernate.envers.Audited
public class Identity extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_FIRST_NAME) @NotNull private String firstName;
	@Column(name = COLUMN_LAST_NAMES) @NotNull private String lastNames;
	@Column(name = COLUMN_ELECTRONIC_MAIL_ADDRESS) @NotNull private String electronicMailAddress;
	
	@Column(name = COLUMN_REGISTRATION_NUMBER) private String registrationNumber;
	@Column(name = COLUMN_POSTAL_BOX_ADDRESS) private String postalBoxAddress;
	@Column(name = COLUMN_MOBILE_PHONE_NUMBER) private String mobilePhoneNumber;
	@Column(name = COLUMN_OFFICE_PHONE_NUMBER) private String officePhoneNumber;
	@Column(name = COLUMN_OFFICE_PHONE_EXTENSION) private String officePhoneExtension;
	@ManyToOne @JoinColumn(name = COLUMN_ADMINISTRATIVE_UNIT) @Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED) private AdministrativeUnit administrativeUnit;
	@Column(name = COLUMN_ADMINISTRATIVE_FUNCTION) private String administrativeFunction;
	@ManyToOne @JoinColumn(name = COLUMN_CIVILITY) @Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED) private Civility civility;
	@ManyToOne @JoinColumn(name = COLUMN_GROUP) @Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED) private IdentityGroup group;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_REFERENCE) private String actOfAppointmentReference;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_SIGNATORY) private String actOfAppointmentSignatory;
	@Column(name = COLUMN_ACT_OF_APPOINTMENT_SIGNATURE_DATE) private LocalDate actOfAppointmentSignatureDate;
	
	@Transient private String names;
	@Transient private String actOfAppointmentSignatureDateAsString;
	@Transient private Long actOfAppointmentSignatureDateAsTimestamp;
	@Transient private String administrativeUnitAsString,sectionAsString;
	
	@Override
	public Identity setIdentifier(String identifier) {
		return (Identity) super.setIdentifier(identifier);
	}
	
	public Identity setAdministrativeUnitFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setAdministrativeUnit(null);
		else
			setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, identifier));
		return this;
	}
	
	public Identity setCivilityFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setCivility(null);
		else
			setCivility(EntityFinder.getInstance().find(Civility.class, identifier));
		return this;
	}
	
	public Identity setGroupFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setGroup(null);
		else
			setGroup(EntityFinder.getInstance().find(IdentityGroup.class, identifier));
		return this;
	}
	
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
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	public static final String FIELD_ADMINISTRATIVE_FUNCTION = "administrativeFunction";
	public static final String FIELD_CIVILITY = "civility";
	public static final String FIELD_CIVILITY_STRING = "civilityAsString";
	public static final String FIELD_GROUP = "group";
	public static final String FIELD_GROUP_AS_STRING = "groupAsString";
	public static final String FIELD_ACT_OF_APPOINTMENT_REFERENCE = "actOfAppointmentReference";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATORY = "actOfAppointmentSignatory";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "actOfAppointmentSignatureDate";
	
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_STRING = "actOfAppointmentSignatureDateAsString";
	public static final String FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_TIMESTAMP = "actOfAppointmentSignatureDateAsTimestamp";
	public static final String FIELD_NAMES = "names";
	
	public static final String TABLE_NAME = "ID_IDENTITE";
	
	public static final String COLUMN_FIRST_NAME = "NOM";
	public static final String COLUMN_LAST_NAMES = "PRENOMS";
	public static final String COLUMN_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
	public static final String COLUMN_REGISTRATION_NUMBER = "MATRICULE";
	public static final String COLUMN_POSTAL_BOX_ADDRESS = "BOITE_POSTALE";
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
		Civility getCivility();
		String getFirstName();
		String getLastNames();
		String getNames();
		IdentityGroup getGroup();
		String getRegistrationNumber();
		String getPostalBoxAddress();
		String getMobilePhoneNumber();
		String getOfficePhoneNumber();
		String getOfficePhoneExtension();
		String getElectronicMailAddress();
		AdministrativeUnit getAdministrativeUnit();
		String getAdministrativeFunction();
		String getActOfAppointmentReference();
		String getActOfAppointmentSignatory();
		LocalDate getActOfAppointmentSignatureDate();
		String getActOfAppointmentSignatureDateAsString();
		Long getActOfAppointmentSignatureDateAsTimestamp();
	}
	
	@Override
	public String toString() {
		return firstName+" "+lastNames+" "+electronicMailAddress;
	}
	
	public static String getNames(String civility,String firstName,String lastNames) {
		String names = null;
		if(StringHelper.isNotBlank(firstName))
			names = firstName;
		if(StringHelper.isNotBlank(lastNames))
			if(StringHelper.isBlank(names))
				names = lastNames;
			else
				names += " "+lastNames;
		if(names == null)
			names = ConstantEmpty.STRING;
		if(StringHelper.isNotBlank(names) && StringHelper.isNotBlank(civility))
			names = civility+" "+names;
		return names;
	}
	
	public static String getNames(Identity identity) {
		return getNames(identity.getCivility() == null ? null : identity.getCivility().getName(), identity.getFirstName(), identity.getLastNames());
	}
	
	public static String getNames(AccountRequest accountRequest) {
		if(accountRequest.getIdentity() == null)
			return getNames(accountRequest.getCivility() == null ? null : accountRequest.getCivility().getName(), accountRequest.getFirstName(), accountRequest.getLastNames());
		return getNames(accountRequest.getIdentity());
	}
	
	public static String getNames(Actor actor) {
		if(actor.getIdentity() == null)
			return getNames(actor.getCivility() == null ? null : actor.getCivility().getName(), actor.getFirstName(), actor.getLastNames());
		return getNames(actor.getIdentity());
	}
	
	public static String getNames(RejectedAccountRequest rejectedAccountRequest) {
		return getNames(null, rejectedAccountRequest.getFirstName(), rejectedAccountRequest.getLastNames());
	}
}