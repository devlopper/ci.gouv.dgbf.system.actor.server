package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ScopeFunctionDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	private ScopeDto scope;
	private String scopeAsString;
	
	private FunctionDto function;
	private String functionAsString;
	private ArrayList<String> functionsIdentifiers;
	
	private LocalityDto locality;
	private String localityAsString;
	
	private Integer numberOfActor;
	private ArrayList<String> actorsAsStrings;
	
	private Boolean shared;
	private String sharedAsString;
	
	private String parentIdentifier;
	private String parentAsString;
	private ArrayList<String> childrenIdentifiers;
	private ArrayList<String> childrenCodesNames;
	
	private Boolean requested;
	private Boolean granted;
	
	/**/
	
	private String signatureSpecimenReadReportURIQuery;
	
	@Override
	public ScopeFunctionDto setIdentifier(String identifier) {
		return (ScopeFunctionDto) super.setIdentifier(identifier);
	}
}