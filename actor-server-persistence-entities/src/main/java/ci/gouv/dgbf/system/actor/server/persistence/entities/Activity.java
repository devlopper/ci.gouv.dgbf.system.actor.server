package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Activity.TABLE_NAME)
public class Activity extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Column(name = COLUMN_SECTION_CODE_NAME) private String sectionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_BUDGET_SPECIALIZATION_UNIT) private BudgetSpecializationUnit budgetSpecializationUnit;
	@Column(name = COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME) private String budgetSpecializationUnitCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTION) private Action action;
	@Column(name = COLUMN_ACTION_CODE_NAME) private String actionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_EXPENDITURE_NATURE) private ExpenditureNature expenditureNature;
	@Column(name = COLUMN_EXPENDITURE_NATURE_CODE_NAME) private String expenditureNatureCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_CATEGORY) private ActivityCategory category;
	@Column(name = COLUMN_CATEGORY_CODE_NAME) private String categoryCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ADMINISTRATIVE_UNIT) private AdministrativeUnit administrativeUnit;
	@Column(name = COLUMN_ADMINISTRATIVE_UNIT_CODE_NAME) private String administrativeUnitCodeName;
	
	@Transient private String sectionIdentifier;
	@Transient private String budgetSpecializationUnitIdentifier;
	@Transient private String actionIdentifier;
	@Transient private String expenditureNatureIdentifier;
	@Transient private String categoryIdentifier;
	@Transient private String administrativeUnitIdentifier;
	
	@Override
	public Activity setIdentifier(String identifier) {
		return (Activity) super.setIdentifier(identifier);
	}
	
	@Override
	public Activity setCode(String code) {
		return (Activity) super.setCode(code);
	}
	
	public Activity setSectionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setSection(null);
		else
			setSection(EntityFinder.getInstance().find(Section.class, identifier));
		return this;
	}
	
	public Activity setBudgetSpecializationUnitFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setBudgetSpecializationUnit(null);
		else
			setBudgetSpecializationUnit(EntityFinder.getInstance().find(BudgetSpecializationUnit.class, identifier));
		return this;
	}
	
	public Activity setActionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setAction(null);
		else
			setAction(EntityFinder.getInstance().find(Action.class, identifier));
		return this;
	}
	
	public Activity setCategoryFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setCategory(null);
		else
			setCategory(EntityFinder.getInstance().find(ActivityCategory.class, identifier));
		return this;
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_CODE_NAME = "sectionCodeName";
	public static final String FIELD_SECTION_IDENTIFIER = "sectionIdentifier";
	public static final String FIELD_CATEGORY = "category";
	public static final String FIELD_CATEGORY_IDENTIFIER = "categoryIdentifier";
	public static final String FIELD_CATEGORY_CODE_NAME = "categoryCodeName";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "budgetSpecializationUnitIdentifier";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "budgetSpecializationUnitCodeName";
	public static final String FIELD_ACTION = "action";
	public static final String FIELD_ACTION_CODE_NAME = "actionCodeName";
	public static final String FIELD_ACTION_IDENTIFIER = "actionIdentifier";
	public static final String FIELD_EXPENDITURE_NATURE = "expenditureNature";
	public static final String FIELD_EXPENDITURE_NATURE_CODE_NAME = "expenditureNatureCodeName";
	public static final String FIELD_EXPENDITURE_NATURE_IDENTIFIER = "expenditureNatureIdentifier";
	public static final String FIELD_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeUnitIdentifier";
	public static final String FIELD_ADMINISTRATIVE_UNIT_CODE_NAME = "administrativeUnitCodeName";
	
	public static final String TABLE_NAME = "VM_APP_ACTIVITE";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_SECTION_CODE_NAME = "SECTION_CODE_LIBELLE";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT = "USB";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "USB_CODE_LIBELLE";
	public static final String COLUMN_ACTION = "ACTION";
	public static final String COLUMN_ACTION_CODE_NAME = "ACTION_CODE_LIBELLE";
	public static final String COLUMN_EXPENDITURE_NATURE = "NATURE_DEPENSE";
	public static final String COLUMN_EXPENDITURE_NATURE_CODE_NAME = "NATURE_DEPENSE_CODE_LIBELLE";
	public static final String COLUMN_CATEGORY = "CATEGORIE";
	public static final String COLUMN_CATEGORY_CODE_NAME = "CATEGORIE_CODE_LIBELLE";
	public static final String COLUMN_ADMINISTRATIVE_UNIT = "UA";
	public static final String COLUMN_ADMINISTRATIVE_UNIT_CODE_NAME = "UA_CODE_LIBELLE";
}