package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	/* Initialization */
	
	private RequestTypeDto type;
	private String typeAsString;
	private RequestStatusDto status;
	private Boolean authenticationRequired;
	private String statusAsString;
	private String authenticationRequiredAsString;
	private String creationDateAsString;
	private String accessToken;
	
	/* Identity */
	
	private ActorDto actor;
	private String actorAsString,actorCode,actorNames;
	private String firstName;
	private String lastNames;
	private String firstNameAndLastNames;
	private String names;
	private String registrationNumber;
	private CivilityDto civility;
	private IdentityGroupDto group;
	private String electronicMailAddress;
	private String postalBoxAddress;
	private String mobilePhoneNumber;
	private String officePhoneNumber;
	private String officePhoneExtension;
	private String photoIdentifier;
	private String signatureIdentifier;
	//private byte[] signedRequestSheetBytes;
	//private byte[] photo;
	
	/* Job */
	
	private Integer budgetaryExercice;
	private AdministrativeUnitDto administrativeUnit;
	private String administrativeUnitAsString;
	private String administrativeFunction;
	private SectionDto section;
	private String sectionAsString;
	private BudgetSpecializationUnitDto budgetSpecializationUnit;
	private String actOfAppointmentReference;
	private String actOfAppointmentSignatory;
	private Long actOfAppointmentSignatureDateAsTimestamp;
	private String actOfAppointmentSignatureDateAsString;
	private String actOfAppointmentIdentifier;
	private String signedRequestSheetIdentifier;
	private ArrayList<FunctionDto> functions;
	private ArrayList<String> functionsAsStrings;
	private ArrayList<FunctionDto> budgetariesFunctions;
	private ArrayList<String> budgetariesFunctionsAsStrings;
	private ArrayList<ScopeFunctionDto> budgetariesScopeFunctions;
	private ArrayList<String> budgetariesScopeFunctionsAsStrings;
	private ArrayList<String> budgetariesScopeFunctionsGrantedAsStrings;
	private Boolean hasGrantedHolderScopeFunction;
	private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeBelongsToExecutionAssistantsCodes;
	private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsCreditManagerHolder;
	private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsAuthorizingOfficerHolder;
	private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsFinancialControllerHolder;
	private Boolean hasBudgetaryScopeFunctionWhereFunctionCodeIsAccountingHolder;
	
	/* Others */
	
	private String comment;
	
	/* Processing */
	
	private String processingDateAsString;
	private Boolean processed;
	private String acceptationComment;
	private Boolean accepted;
	private String rejectionReason;
	private Boolean rejected;
	
	/* Report identifier */
	
	private String readReportURIQuery;
	private String signatureSpecimenReadReportURIQuery;
	private String creditManagerSignatureSpecimenReadReportURIQuery;
	private ArrayList<String> creditManagerSignatureSpecimenReadReportURIsQueries;
	private String authorizingOfficerSignatureSpecimenReadReportURIQuery;
	private ArrayList<String> authorizingOfficerSignatureSpecimenReadReportURIsQueries;
	
	/**/
	
	private String readPageURL;
	private String signatureSpecimenReadPageURL;
	private Boolean isAdministrator;
	
	/**/
	
	@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
	public static class Acceptation {
		private String identifier;
		private String comment;
	}
	
	@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
	public static class Rejection {
		private String identifier;
		private String reason;
	}
}