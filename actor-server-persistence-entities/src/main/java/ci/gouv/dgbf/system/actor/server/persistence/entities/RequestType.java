package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestType.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class RequestType extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_FORM) @NotNull private IdentificationForm form;
	@Transient private String formAsString;
	@Column(name = COLUMN_REPORT_IDENTIFIER) private String reportIdentifier;
	@Column(name = COLUMN_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER) private String signatureSpecimenReportIdentifier;
	@Column(name = COLUMN_CREDIT_MANAGER_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER) private String creditManagerSignatureSpecimenReportIdentifier;
	@Column(name = COLUMN_AUTHORIZING_OFFICER_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER) private String authorizingOfficerSignatureSpecimenReportIdentifier;
	@Column(name = COLUMN_AUTHENTICATION_REQUIRED) private Boolean authenticationRequired;
	@Transient private String authenticationRequiredAsString;
	@Column(name = COLUMN_NOTIFIABLE_BY_EMAIL) private Boolean notifiableByEmail;
	@Transient private String notifiableByEmailAsString;
	
	@Override
	public RequestType setIdentifier(String identifier) {
		return (RequestType) super.setIdentifier(identifier);
	}
	
	@Override
	public RequestType setCode(String code) {
		return (RequestType) super.setCode(code);
	}
	
	public RequestType setFormFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setForm(null);
		else
			setForm(EntityFinder.getInstance().find(IdentificationForm.class, identifier));
		return this;
	}
	
	public static final String FIELD_FORM = "form";
	public static final String FIELD_FORM_AS_STRING = "formAsString";
	public static final String FIELD_REPORT_IDENTIFIER = "reportIdentifier";
	public static final String FIELD_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER = "signatureSpecimenReportIdentifier";
	public static final String FIELD_AUTHENTICATION_REQUIRED = "authenticationRequired";		
	public static final String FIELD_AUTHENTICATION_REQUIRED_AS_STRING = "authenticationRequiredAsString";
	public static final String FIELD_NOTIFIABLE_BY_EMAIL = "notifiableByEmail";
	public static final String FIELD_NOTIFIABLE_BY_EMAIL_AS_STRING = "notifiableByEmailAsString";
	
	public static final String TABLE_NAME = "DM_TYPE";
	
	public static final String COLUMN_FORM = "FORMULAIRE";
	public static final String COLUMN_REPORT_IDENTIFIER = "ETAT_IDENTIFIANT";
	public static final String COLUMN_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER = "ETAT_SPECIMEN_SIGNATURE_ID";
	public static final String COLUMN_CREDIT_MANAGER_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER = "ETAT_SPECIMEN_SIGNATURE_GC_ID";
	public static final String COLUMN_AUTHORIZING_OFFICER_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER = "ETAT_SPECIMEN_SIGNATURE_ORD_ID";
	public static final String COLUMN_AUTHENTICATION_REQUIRED = "AUTHENTIFICATION_REQUISE";
	public static final String COLUMN_NOTIFIABLE_BY_EMAIL = "NOTIFIABLE_PAR_EMAIL";
	
	public static final String CODE_DEMANDE_POSTES_BUDGETAIRES = "DEMANDE_POSTES_BUDGETAIRES";
	public static final String IDENTIFIER_DEMANDE_POSTES_BUDGETAIRES = "DPB";
}