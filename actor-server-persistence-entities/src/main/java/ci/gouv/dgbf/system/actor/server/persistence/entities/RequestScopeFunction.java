package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestScopeFunction.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = RequestScopeFunction.FIELD___AUDIT_WHO__,column = @Column(name="AUDIT_ACTEUR"))
		,@AttributeOverride(name = RequestScopeFunction.FIELD___AUDIT_WHAT__,column = @Column(name="AUDIT_ACTION"))
		,@AttributeOverride(name = RequestScopeFunction.FIELD___AUDIT_WHEN__,column = @Column(name="AUDIT_DATE"))
		,@AttributeOverride(name = RequestScopeFunction.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name="AUDIT_FONCTIONNALITE"))
})
public class RequestScopeFunction extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_REQUEST) @NotNull private Request request;
	@Transient private String requestIdentifier;
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE_FUNCTION) @NotNull private ScopeFunction scopeFunction;
	@Transient private String scopeFunctionIdentifier;
	@Transient private String scopeFunctionCode;
	@Transient private String scopeFunctionName;
	@Transient private String functionCode;
	@Column(name = COLUMN_REQUESTED) private Boolean requested;
	@Column(name = COLUMN_GRANTED) private Boolean granted;
	
	
	@Transient private String electronicMailAddress;
	@Transient private String scopeFunctionString;
	@Transient private String grantedString;
	@Transient private String sectionString;
	@Transient private String administrativeUnitString;
	@Transient private String administrativeUnitFunction;
	@Transient private String mobilePhoneNumber;
	@Transient private String officePhoneNumber;
	@Transient private String postalBoxAddress;
	@Transient private String actOfAppointmentReference;
	@Transient private String firstName;
	@Transient private String lastNames;
	@Transient private String registrationNumber;
	@Transient private String signatureSpecimenReportIdentifier;
	
	@Override
	public RequestScopeFunction setIdentifier(String identifier) {
		return (RequestScopeFunction) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_REQUEST = "request";
	public static final String FIELD_REQUEST_IDENTIFIER = "requestIdentifier";
	public static final String FIELD_SCOPE_FUNCTION = "scopeFunction";
	public static final String FIELD_SCOPE_FUNCTION_IDENTIFIER = "scopeFunctionIdentifier";
	public static final String FIELD_SCOPE_FUNCTION_CODE = "scopeFunctionCode";
	public static final String FIELD_SCOPE_FUNCTION_NAME = "scopeFunctionName";
	public static final String FIELD_FUNCTION_CODE = "functionCode";
	public static final String FIELD_REQUESTED = "requested";
	public static final String FIELD_GRANTED = "granted";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_SCOPE_FUNCTION_STRING = "scopeFunctionString";
	public static final String FIELD_GRANTED_STRING = "grantedString";
	public static final String FIELDS_IDENTITY_SCOPE_FUNCTION_STRING_GRANTED_STRING = "identityScopeFunctionStringGrantedString";
	
	public static final String TABLE_NAME = "DM_POSTE";
	
	public static final String COLUMN_REQUEST = "DEMANDE";
	public static final String COLUMN_SCOPE_FUNCTION = "POSTE";
	public static final String COLUMN_REQUESTED = "DEMANDEE";
	public static final String COLUMN_GRANTED = "ACCORDEE";
	
	public static final String LABEL = "Poste";
}