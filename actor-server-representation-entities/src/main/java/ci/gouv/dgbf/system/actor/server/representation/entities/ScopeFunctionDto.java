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

	private String codePrefix;
	
	private ScopeDto scope;
	private String scopeIdentifier;
	private String scopeAsString;
	
	private FunctionDto function;
	private String functionIdentifier;
	private String functionCode;
	private String functionAsString;
	private ArrayList<String> functionsIdentifiers;
	private Boolean isHolder;
	
	private LocalityDto locality;
	private String localityIdentifier;
	private String localityAsString;
	
	private Integer numberOfActor;
	private ArrayList<String> actorsNames;
	private ArrayList<String> actorsAsStrings;
	private ArrayList<String> actorsCodes;
	
	private Boolean shared;
	private String sharedAsString;
	
	private String parentIdentifier;
	private String parentAsString;
	private ArrayList<String> childrenIdentifiers;
	private ArrayList<String> childrenCodesNames;
	
	private Boolean requested;
	private String requestedAsString;
	private Boolean granted;
	private String grantedAsString;
	
	private String actorAsString;
	private String assignmentToActorMessage;
	
	private String budgetCategoryAsString;
	private String budgetCategoryIdentifier;
	private String budgetCategoryCode;
	/**/
	
	private String signatureSpecimenReadReportURIQuery;
	
	private ArrayList<ScopeFunctionDto> __auditRecords__;
	
	@Override
	public ScopeFunctionDto setIdentifier(String identifier) {
		return (ScopeFunctionDto) super.setIdentifier(identifier);
	}
	
	@Override
	public ScopeFunctionDto setName(String name) {
		return (ScopeFunctionDto) super.setName(name);
	}
}