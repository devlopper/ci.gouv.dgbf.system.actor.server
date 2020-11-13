package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.List;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class AssignmentsDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	/* Imputation */
	
	private String sectionAsString;
	private String budgetSpecializationUnitAsString;
	private String actionAsString;
	private String activityAsString;
	private String economicNatureAsString;
	private String administrativeUnitAsString;
	private String activityCategoryAsString;
	private String expenditureNatureAsString;
	
	/* Affectations */
	
	private ScopeFunctionDto creditManagerHolder;
	private ScopeFunctionDto creditManagerAssistant;	
	private ScopeFunctionDto authorizingOfficerHolder;
	private ScopeFunctionDto authorizingOfficerAssistant;	
	private ScopeFunctionDto financialControllerHolder;
	private ScopeFunctionDto financialControllerAssistant;	
	private ScopeFunctionDto accountingHolder;
	private ScopeFunctionDto accountingAssistant;
	
	private String creditManagerHolderAsString;
	private String creditManagerAssistantAsString;	
	private String authorizingOfficerHolderAsString;
	private String authorizingOfficerAssistantAsString;	
	private String financialControllerHolderAsString;
	private String financialControllerAssistantAsString;	
	private String accountingHolderAsString;
	private String accountingAssistantAsString;
	
	private Filter.Dto filter;
	private List<String> overridablesFieldsNames;
	
	@Override
	public AssignmentsDto setIdentifier(String identifier) {
		return (AssignmentsDto) super.setIdentifier(identifier);
	}
}