package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Imputation.TABLE_NAME)
public class Imputation extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Column(name = COLUMN_SECTION_CODE_NAME) private String sectionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_BUDGET_SPECIALIZATION_UNIT) private BudgetSpecializationUnit budgetSpecializationUnit;
	@Column(name = COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME) private String budgetSpecializationUnitCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTION) private Action action;
	@Column(name = COLUMN_ACTION_CODE_NAME) private String actionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_ACTIVITY) private Activity activity;
	@Column(name = COLUMN_ACTIVITY_CODE_NAME) private String activityCodeName;
	
	@Column(name = COLUMN_ECONOMIC_NATURE_CODE_NAME) private String economicNatureCodeName;
	
	@Override
	public Imputation setIdentifier(String identifier) {
		return (Imputation) super.setIdentifier(identifier);
	}
	
	@Override
	public Imputation setCode(String code) {
		return (Imputation) super.setCode(code);
	}
	
	public Imputation setSectionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setSection(null);
		else
			setSection(EntityFinder.getInstance().find(Section.class, identifier));
		return this;
	}
	
	public Imputation setBudgetSpecializationUnitFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setBudgetSpecializationUnit(null);
		else
			setBudgetSpecializationUnit(EntityFinder.getInstance().find(BudgetSpecializationUnit.class, identifier));
		return this;
	}
	
	public Imputation setActivityFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setActivity(null);
		else
			setActivity(EntityFinder.getInstance().find(Activity.class, identifier));
		return this;
	}
	
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_CODE_NAME = "sectionCodeName";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "budgetSpecializationUnitCodeName";
	public static final String FIELD_ACTION = "action";
	public static final String FIELD_ACTION_CODE_NAME = "actionCodeName";
	public static final String FIELD_ACTIVITY = "activity";
	public static final String FIELD_ACTIVITY_CODE_NAME = "activityCodeName";
	public static final String FIELD_ECONOMIC_NATURE_CODE_NAME = "economicNatureCodeName";
	
	public static final String TABLE_NAME = "VM_APP_IMPUTATION";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_SECTION_CODE_NAME = "SECTION_CODE_LIBELLE";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT = "USB";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "USB_CODE_LIBELLE";
	public static final String COLUMN_ACTION = "ACTION";
	public static final String COLUMN_ACTION_CODE_NAME = "ACTION_CODE_LIBELLE";
	public static final String COLUMN_ACTIVITY = "ACTIVITE";
	public static final String COLUMN_ACTIVITY_CODE_NAME = "ACTIVITE_CODE_LIBELLE";
	public static final String COLUMN_ECONOMIC_NATURE_CODE_NAME = "NATURE_ECONOMIQUE_CODE_LIBELLE";
}