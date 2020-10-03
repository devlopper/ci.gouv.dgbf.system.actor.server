package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
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
@Entity @Table(name=Menu.TABLE_NAME)
@Cacheable(value = true)
public class Menu extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SERVICE) private Service service;
	@Column(name = COLUMN_SERVICE_CODE_NAME) private String serviceCodeName;
	@Column(name = COLUMN_MODULE_CODE_NAME) private String moduleCodeName;
	
	@Column(name = COLUMN_UNIFORM_RESOURCE_IDENTIFIER)
	private String uniformResourceIdentifier;
	
	@Transient private Boolean defined;
	@Transient private String status;
	@Transient private String serviceAsString;
	@Transient private String moduleAsString;
	@Transient private String profilesAsString;
	
	@Override
	public Menu setIdentifier(String identifier) {
		return (Menu) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_MODULE_AS_STRING = "moduleAsString";
	public static final String FIELD_MODULE_CODE_NAME = "moduleCodeName";
	public static final String FIELD_SERVICE = "service";
	public static final String FIELD_SERVICE_CODE_NAME = "serviceCodeName";
	public static final String FIELD_UNIFORM_RESOURCE_IDENTIFIER = "uniformResourceIdentifier";
	public static final String FIELD_DEFINED = "defined";
	public static final String FIELD_STATUS = "status";
	public static final String FIELD_PROFILES_AS_STRING = "profilesAsString";
	
	public static final String COLUMN_UNIFORM_RESOURCE_IDENTIFIER = "URL";
	public static final String COLUMN_SERVICE = "SERVICE";
	public static final String COLUMN_SERVICE_CODE_NAME = "SERVICE_CODE_LIBELLE";
	public static final String COLUMN_MODULE_CODE_NAME = "MODULE_CODE_LIBELLE";
	
	public static final String TABLE_NAME = "VM_APP_MENU";
	
	public static final String __ROOT__NAME = "__root__";
	public static final String __ROOT__URL = "/*";
}