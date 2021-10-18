package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestDispatchSlipDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private SectionDto section;
	private String sectionIdentifier;
	private String sectionAsString;
	
	private FunctionDto function;
	private String functionIdentifier;
	private String functionAsString;
	
	private String creationDateAsString,sendingDateAsString,processingDateAsString,comment;
	
	private ArrayList<RequestDto> requests;
	private Integer numberOfRequests,numberOfRequestsProcessed,numberOfRequestsAccepted,numberOfRequestsRejected,numberOfRequestsNotProcessed;
	private Boolean isNumberOfRequestsEqualNumberOfRequestsProcessed;
	private ArrayList<String> requestsIdentifiers;
	private ArrayList<RequestDto.Acceptation> requestsAcceptations;
	private ArrayList<RequestDto.Rejection> requestsRejections;
	
	@Override
	public RequestDispatchSlipDto setIdentifier(String identifier) {
		return (RequestDispatchSlipDto) super.setIdentifier(identifier);
	}
	
	/**/
	
}