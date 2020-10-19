package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

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
	
	private ExecutionImputationScopeFunctionDto creditManager;	
	private ExecutionImputationScopeFunctionDto authorizingOfficer;	
	private ExecutionImputationScopeFunctionDto financialController;
	private ExecutionImputationScopeFunctionDto accounting;
	
	private ArrayList<Function> functions;
	
	@Override
	public ExecutionImputationDto setIdentifier(String identifier) {
		return (ExecutionImputationDto) super.setIdentifier(identifier);
	}
}