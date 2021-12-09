package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestScopeFunctionDto extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private String requestIdentifier;
	private String scopeFunctionIdentifier;
	
	private ArrayList<String> scopeFunctionsIdentifiers;
	private String electronicMailAddress;
	private String scopeFunctionString;
	private String sectionString;
	private String administrativeUnitString;
	private String firstName;
	private String lastNames;
	private String grantedString;
	private Boolean granted;
	private String administrativeUnitFunction;
	private String mobilePhoneNumber;
	private String officePhoneNumber;
	private String postalBoxAddress;
	private String actOfAppointmentReference;
	private String registrationNumber;
	private String signatureSpecimenReportIdentifier;
	private String signatureSpecimenReadReportURIQuery;
	
	@Override
	public RequestScopeFunctionDto setIdentifier(String identifier) {
		return (RequestScopeFunctionDto) super.setIdentifier(identifier);
	}

}