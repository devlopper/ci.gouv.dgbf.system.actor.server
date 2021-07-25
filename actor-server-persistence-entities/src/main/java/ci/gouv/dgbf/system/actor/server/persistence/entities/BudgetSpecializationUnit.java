package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.EntityFinder;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=BudgetSpecializationUnit.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class BudgetSpecializationUnit extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@Column(name = COLUMN_SECTION_IDENTIFIER) private String sectionIdentifier;
	@Column(name = COLUMN_SECTION_CODE_NAME) private String sectionCodeName;
	
	@ManyToOne @JoinColumn(name = COLUMN_CATEGORY) private BudgetCategory category;
	@Column(name = COLUMN_CATEGORY_IDENTIFIER) private String categoryIdentifier;
	@Column(name = COLUMN_CATEGORY_CODE_NAME) private String categoryCodeName;
	
	@Override
	public BudgetSpecializationUnit setIdentifier(String identifier) {
		return (BudgetSpecializationUnit) super.setIdentifier(identifier);
	}
	
	@Override
	public BudgetSpecializationUnit setCode(String code) {
		return (BudgetSpecializationUnit) super.setCode(code);
	}
	
	@Override
	public BudgetSpecializationUnit setName(String name) {
		return (BudgetSpecializationUnit) super.setName(name);
	}
	
	public BudgetSpecializationUnit setSectionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setSection(null);
		else
			setSection(EntityFinder.getInstance().find(Section.class, identifier));
		return this;
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_IDENTIFIER = "sectionIdentifier";
	public static final String FIELD_SECTION_CODE_NAME = "sectionCodeName";
	public static final String FIELD_CATEGORY = "category";
	public static final String FIELD_CATEGORY_IDENTIFIER = "categoryIdentifier";
	public static final String FIELD_CATEGORY_CODE_NAME = "categoryCodeName";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_SECTION_IDENTIFIER = "SECTION_IDENTIFIANT";
	public static final String COLUMN_SECTION_CODE_NAME = "SECTION_CODE_LIBELLE";
	public static final String COLUMN_CATEGORY = "CATEGORIE";
	public static final String COLUMN_CATEGORY_IDENTIFIER = "CATEGORIE_IDENTIFIANT";
	public static final String COLUMN_CATEGORY_CODE_NAME = "CATEGORIE_CODE_LIBELLE";
	
	public static final String TABLE_NAME = "VM_APP_USB";
}