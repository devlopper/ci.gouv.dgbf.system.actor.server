package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActivityDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private SectionDto section;
	private String sectionIdentifier;
	private String sectionCodeName;
	
	private BudgetSpecializationUnitDto budgetSpecializationUnit;
	private String budgetSpecializationUnitIdentifier;
	private String budgetSpecializationUnitCodeName;
	
	private ActionDto action;
	private String actionIdentifier;
	private String actionCodeName;
	
	private ExpenditureNatureDto expenditureNature;
	private String expenditureNatureIdentifier;
	private String expenditureNatureCodeName;
	
	private ActivityCategory category;
	private String categoryIdentifier;
	private String categoryCodeName;
	
	private AdministrativeUnitDto administrativeUnit;
	private String administrativeUnitIdentifier;
	private String administrativeUnitCodeName;
}