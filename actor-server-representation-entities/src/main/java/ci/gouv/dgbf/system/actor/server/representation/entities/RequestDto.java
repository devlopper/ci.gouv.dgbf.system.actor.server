package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	/* Initialization */
	
	private RequestTypeDto type;
	private String typeAsString;
	private RequestStatusDto status;
	private Boolean authenticationRequired;
	private String statusAsString;
	private String authenticationRequiredAsString;
	private String creationDateAsString;
	
	/* Identity */
	
	private ActorDto actor;
	private String actorAsString,actorCode,actorNames;
	private String firstName;
	private String lastNames;
	private String registrationNumber;
	private CivilityDto civility;
	private IdentityGroupDto group;
	private String electronicMailAddress;
	private String postalBoxAddress;
	private String mobilePhoneNumber;
	private String officePhoneNumber;
	private String officePhoneExtension;
	
	/* Job */
	
	private Integer budgetaryExercice;
	private AdministrativeUnitDto administrativeUnit;
	private String administrativeFunction;
	private SectionDto section;
	private BudgetSpecializationUnitDto budgetSpecializationUnit;
	private String actOfAppointmentReference;
	private String actOfAppointmentSignatory;
	private Long actOfAppointmentSignatureDateAsTimestamp;
	private String actOfAppointmentSignatureDateAsString;
	private ArrayList<FunctionDto> functions;
	private ArrayList<String> functionsAsStrings;
	private ArrayList<FunctionDto> budgetariesFunctions;
	private ArrayList<String> budgetariesFunctionsAsStrings;
	private ArrayList<ScopeFunctionDto> budgetariesScopeFunctions;
	private ArrayList<String> budgetariesScopeFunctionsAsStrings;
	
	/* Others */
	
	private String comment;
	
	/* Processing */
	
	private String processingDateAsString;
	private String rejectionReason;
	
	/* Uniform Resource Identifier : URI*/
	
	private String reportUniformResourceIdentifier;
}