package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Locality.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class Locality extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_TYPE) @Enumerated(EnumType.STRING) private Type type;
	@ManyToOne @JoinColumn(name = COLUMN_PARENT) private Locality parent;
	
	@Column(name = COLUMN_REGION_IDENTIFIER) private String regionIdentifier;
	@Column(name = COLUMN_REGION_CODE_NAME) private String regionCodeName;
	
	@Column(name = COLUMN_DEPARTMENT_IDENTIFIER) private String departmentIdentifier;
	@Column(name = COLUMN_DEPARTMENT_CODE_NAME) private String departmentCodeName;
	
	@Transient private Locality region;
	@Transient private Locality department;
	
	@Override
	public Locality setIdentifier(String identifier) {
		return (Locality) super.setIdentifier(identifier);
	}
	
	@Override
	public Locality setCode(String code) {
		return (Locality) super.setCode(code);
	}
	
	@Override
	public Locality setName(String name) {
		return (Locality) super.setName(name);
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_PARENT = "parent";
	public static final String FIELD_REGION_IDENTIFIER = "regionIdentifier";
	public static final String FIELD_REGION_CODE_NAME = "regionCodeName";
	public static final String FIELD_DEPARTMENT_IDENTIFIER = "departmentIdentifier";
	public static final String FIELD_DEPARTMENT_CODE_NAME = "departmentCodeName";
	
	public static final String TABLE_NAME = "VM_APP_LOCALITE";
	
	public static final String COLUMN_TYPE = "type";
	public static final String COLUMN_PARENT = "parent";
	public static final String COLUMN_REGION_IDENTIFIER = "region_identifiant";
	public static final String COLUMN_REGION_CODE_NAME = "region_code_libelle";
	public static final String COLUMN_DEPARTMENT_IDENTIFIER = "departement_identifiant";
	public static final String COLUMN_DEPARTMENT_CODE_NAME = "departement_code_libelle";
	
	public static final String CODE_SOUS_PREFECTURE_BINGERVILLE = "780102";
	
	public static enum Type {REGION,DEPARTEMENT,SOUS_PREFECTURE}
}