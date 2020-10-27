package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ExecutionImputationDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private String sectionCodeName;
	private String budgetSpecializationUnitCodeName;
	private String actionCodeName;
	private String activityCodeName;
	private String economicNatureCodeName;
	private String administrativeUnitCodeName;
	private String activityCategoryCodeName;
	private String expenditureNatureCodeName;
	
	private String creditManagerHolderCodeName;
	private String creditManagerAssistantCodeName;
	
	private String authorizingOfficerHolderCodeName;
	private String authorizingOfficerAssistantCodeName;
	
	private String financialControllerHolderCodeName;
	private String financialControllerAssistantCodeName;
	
	private String accountingHolderCodeName;
	private String accountingAssistantCodeName;
	
	private ExecutionImputationScopeFunctionDto creditManager;	
	private ExecutionImputationScopeFunctionDto authorizingOfficer;	
	private ExecutionImputationScopeFunctionDto financialController;
	private ExecutionImputationScopeFunctionDto accounting;
	
	private ArrayList<Function> functions;
	
	private Filter.Dto filter;
	
	@Override
	public ExecutionImputationDto setIdentifier(String identifier) {
		return (ExecutionImputationDto) super.setIdentifier(identifier);
	}
}