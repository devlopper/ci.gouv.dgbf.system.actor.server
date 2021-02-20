package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
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
@Entity @Table(name=Action.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class Action extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Column(name = COLUMN_SECTION_CODE_NAME) private String sectionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_BUDGET_SPECIALIZATION_UNIT) private BudgetSpecializationUnit budgetSpecializationUnit;
	@Column(name = COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME) private String budgetSpecializationUnitCodeName;
	
	@Override
	public Action setIdentifier(String identifier) {
		return (Action) super.setIdentifier(identifier);
	}
	
	@Override
	public Action setCode(String code) {
		return (Action) super.setCode(code);
	}
	
	@Override
	public Action setName(String name) {
		return (Action) super.setName(name);
	}
	
	public Action setSectionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setSection(null);
		else
			setSection(EntityFinder.getInstance().find(Section.class, identifier));
		return this;
	}
	
	public Action setBudgetSpecializationUnitFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setBudgetSpecializationUnit(null);
		else
			setBudgetSpecializationUnit(EntityFinder.getInstance().find(BudgetSpecializationUnit.class, identifier));
		return this;
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_CODE_NAME = "sectionCodeName";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "budgetSpecializationUnitCodeName";
	
	public static final String TABLE_NAME = "VM_APP_ACTION";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_SECTION_CODE_NAME = "SECTION_CODE_LIBELLE";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT = "USB";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT_CODE_NAME = "USB_CODE_LIBELLE";
}