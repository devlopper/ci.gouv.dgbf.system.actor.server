package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=AdministrativeUnit.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class AdministrativeUnit extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) @NotNull private Section section;
	@Column(name= COLUMN_SECTION_CODE_NAME) private String sectionCodeName;
	@Transient private String sectionAsString;
	@Transient private String sectionIdentifier;
	
	@Override
	public AdministrativeUnit setIdentifier(String identifier) {
		return (AdministrativeUnit) super.setIdentifier(identifier);
	}
	
	@Override
	public AdministrativeUnit setCode(String code) {
		return (AdministrativeUnit) super.setCode(code);
	}
	
	@Override
	public AdministrativeUnit setName(String name) {
		return (AdministrativeUnit) super.setName(name);
	}
	
	public AdministrativeUnit setSectionFromIdentifier(String identifier) {
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
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_SECTION_CODE_NAME = "SECTION_CODE_LIBELLE";
	
	public static final String TABLE_NAME = "VM_APP_UNITE_ADMINISTRATIVE";	
}