package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestTypeDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private IdentificationFormDto form;
	private String reportIdentifier;
	private String signatureSpecimenReportIdentifier;
	private String creditManagerSignatureSpecimenReportIdentifier;
	private String authorizingOfficerSignatureSpecimenReportIdentifier;
	private Boolean authenticationRequired;
	private String formAsString;
	private String authenticationRequiredAsString;
	
	@Override
	public RequestTypeDto setIdentifier(String identifier) {
		return (RequestTypeDto) super.setIdentifier(identifier);
	}

}