package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringAuditedImpl;
import org.cyk.utility.persistence.query.Filter;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class AssignmentsDto extends AbstractIdentifiableSystemScalarStringAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	/* Imputation */
	
	private String budgetCategoryAsString;
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
	private String accountingTresorIdentifier;
	
	private Filter.Dto filter;
	private List<String> overridablesFieldsNames;
	private Boolean overridable,holdersSettable,assistantsSettable;
	
	@Override
	public AssignmentsDto setIdentifier(String identifier) {
		return (AssignmentsDto) super.setIdentifier(identifier);
	}
	
	private ArrayList<AssignmentsDto> __auditRecords__;
}