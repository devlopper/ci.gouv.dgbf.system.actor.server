package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@MappedSuperclass
public abstract class AbstractImputation extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Column(name = COLUMN_SECTION_CODE) private String sectionCode;
	@Column(name = COLUMN_SECTION_CODE_NAME) private String sectionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_BUDGET_SPECIALIZATION_UNIT) private BudgetSpecializationUnit budgetSpecializationUnit;
	@Column(name = COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE) private String budgetSpecializationUnitCode;
	@Column(name = COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME) private String budgetSpecializationUnitCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTION) private Action action;
	@Column(name = COLUMN_ACTION_CODE) private String actionCode;
	@Column(name = COLUMN_ACTION_CODE_NAME) private String actionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTIVITY) private Activity activity;
	@Column(name = COLUMN_ACTIVITY_CODE) private String activityCode;
	@Column(name = COLUMN_ACTIVITY_CODE_NAME) private String activityCodeName;
	@Column(name = COLUMN_ACTIVITY_LOCALITY_CODE) private String activityLocalityCode;
	
	@Column(name = COLUMN_ECONOMIC_NATURE_CODE) private String economicNatureCode;
	@Column(name = COLUMN_ECONOMIC_NATURE_CODE_NAME) private String economicNatureCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ADMINISTRATIVE_UNIT) private AdministrativeUnit administrativeUnit;
	@Column(name = COLUMN_ADMINISTRATIVE_UNIT_CODE) private String administrativeUnitCode;
	@Column(name = COLUMN_ADMINISTRATIVE_UNIT_CODE_NAME) private String administrativeUnitCodeName;	
	@Column(name = COLUMN_ADMINISTRATIVE_UNIT_LOCALITY_CODE) private String administrativeUnitLocalityCode;
	
	@ManyToOne @JoinColumn(name = COLUMN_MANAGER) private AdministrativeUnit manager;
	@Column(name = COLUMN_MANAGER_CODE) private String managerCode;
	@Column(name = COLUMN_MANAGER_CODE_NAME) private String managerCodeName;	
	@Column(name = COLUMN_MANAGER_LOCALITY_CODE) private String managerLocalityCode;
	
	@Column(name = COLUMN_ACTIVITY_CATEGORY_CODE_NAME) private String activityCategoryCodeName;
	
	@Column(name = COLUMN_EXPENDITURE_NATURE_CODE_NAME) private String expenditureNatureCodeName;
	
	@Override
	public AbstractImputation setIdentifier(String identifier) {
		return (AbstractImputation) super.setIdentifier(identifier);
	}
	
	@Override
	public AbstractImputation setCode(String code) {
		return (AbstractImputation) super.setCode(code);
	}
	
	public AbstractImputation setSectionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setSection(null);
		else
			setSection(EntityFinder.getInstance().find(Section.class, identifier));
		return this;
	}
	
	public AbstractImputation setBudgetSpecializationUnitFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setBudgetSpecializationUnit(null);
		else
			setBudgetSpecializationUnit(EntityFinder.getInstance().find(BudgetSpecializationUnit.class, identifier));
		return this;
	}
	
	public AbstractImputation setActivityFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setActivity(null);
		else
			setActivity(EntityFinder.getInstance().find(Activity.class, identifier));
		return this;
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_CODE = "sectionCode";
	public static final String FIELD_SECTION_CODE_NAME = "sectionCodeName";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_CODE = "budgetSpecializationUnitCode";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "budgetSpecializationUnitCodeName";
	public static final String FIELD_ACTION = "action";
	public static final String FIELD_ACTION_CODE = "actionCode";
	public static final String FIELD_ACTION_CODE_NAME = "actionCodeName";
	public static final String FIELD_ACTIVITY = "activity";
	public static final String FIELD_ACTIVITY_CODE = "activityCode";
	public static final String FIELD_ACTIVITY_CODE_NAME = "activityCodeName";
	public static final String FIELD_ACTIVITY_LOCALITY_CODE = "activityLocalityCode";
	public static final String FIELD_ECONOMIC_NATURE_CODE = "economicNatureCode";
	public static final String FIELD_ECONOMIC_NATURE_CODE_NAME = "economicNatureCodeName";
	public static final String FIELD_ADMINISTRATIVE_UNIT = "administrativeUnit";
	public static final String FIELD_ADMINISTRATIVE_UNIT_CODE = "administrativeUnitCode";
	public static final String FIELD_ADMINISTRATIVE_UNIT_CODE_NAME = "administrativeUnitCodeName";
	
	public static final String FIELD_MANAGER = "manager";
	public static final String FIELD_MANAGER_CODE = "managerCode";
	public static final String FIELD_MANAGER_CODE_NAME = "managerCodeName";
	public static final String FIELD_MANAGER_LOCALITY_CODE = "managerLocalityCode";
	
	public static final String FIELD_ADMINISTRATIVE_UNIT_LOCALITY_CODE = "administrativeUnitLocalityCode";
	public static final String FIELD_ACTIVITY_CATEGORY_CODE_NAME = "activityCategoryCodeName";
	public static final String FIELD_EXPENDITURE_NATURE_CODE_NAME = "expenditureNatureCodeName";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_SECTION_CODE = "SECTION_CODE";
	public static final String COLUMN_SECTION_CODE_NAME = "SECTION_CODE_LIBELLE";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT = "USB";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE = "USB_CODE";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "USB_CODE_LIBELLE";
	public static final String COLUMN_ACTION = "ACTION";
	public static final String COLUMN_ACTION_CODE = "ACTION_CODE";
	public static final String COLUMN_ACTION_CODE_NAME = "ACTION_CODE_LIBELLE";
	public static final String COLUMN_ACTIVITY = "ACTIVITE";
	public static final String COLUMN_ACTIVITY_CODE = "ACTIVITE_CODE";
	public static final String COLUMN_ACTIVITY_CODE_NAME = "ACTIVITE_CODE_LIBELLE";
	public static final String COLUMN_ACTIVITY_LOCALITY_CODE = "ACTIVITE_LOCALITE_CODE";
	public static final String COLUMN_ECONOMIC_NATURE_CODE = "NATURE_ECONOMIQUE_CODE";
	public static final String COLUMN_ECONOMIC_NATURE_CODE_NAME = "NATURE_ECONOMIQUE_CODE_LIBELLE";
	public static final String COLUMN_ADMINISTRATIVE_UNIT = "UA";
	public static final String COLUMN_ADMINISTRATIVE_UNIT_CODE = "UA_CODE";
	public static final String COLUMN_ADMINISTRATIVE_UNIT_CODE_NAME = "UA_CODE_LIBELLE";
	
	public static final String COLUMN_MANAGER = "GESTIONNAIRE";
	public static final String COLUMN_MANAGER_CODE = "GESTIONNAIRE_CODE";
	public static final String COLUMN_MANAGER_CODE_NAME = "GESTIONNAIRE_CODE_LIBELLE";
	public static final String COLUMN_MANAGER_LOCALITY_CODE = "GESTIONNAIRE_LOCALITE_CODE";
	
	public static final String COLUMN_ADMINISTRATIVE_UNIT_LOCALITY_CODE = "UA_LOCALITE_CODE";
	
	public static final String COLUMN_ACTIVITY_CATEGORY_CODE_NAME = "CA_CODE_LIBELLE";
	public static final String COLUMN_EXPENDITURE_NATURE_CODE_NAME = "ND_CODE_LIBELLE";
}