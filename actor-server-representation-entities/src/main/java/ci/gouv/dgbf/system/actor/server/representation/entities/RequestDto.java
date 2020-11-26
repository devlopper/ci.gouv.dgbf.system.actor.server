package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class RequestDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	private RequestTypeDto type;
	private ActorDto actor;
	private String comment;
	private ArrayList<FunctionDto> functions;
	private AdministrativeUnitDto administrativeUnit;
	private SectionDto section;
	private BudgetSpecializationUnitDto budgetSpecializationUnit;
	private String administrativeFunction;
	private RequestStatusDto status;
	private String statusAsString;
	private String rejectionReason;
	
	private String actOfAppointmentReference;
	private String actOfAppointmentSignatory;
	private Long actOfAppointmentSignatureDateAsTimestamp;
	private String actOfAppointmentSignatureDateAsString;
	
	private ArrayList<String> functionsAsStrings;	
	private String typeAsString,actorAsString,actorCode,actorNames,creationDateAsString,processingDateAsString;
}