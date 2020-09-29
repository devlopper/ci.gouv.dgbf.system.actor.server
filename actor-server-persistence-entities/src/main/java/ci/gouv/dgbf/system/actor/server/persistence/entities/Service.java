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
@Entity @Table(name=Service.TABLE_NAME)
@Cacheable(value = true)
public class Service extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_MODULE) private Module module;
	@Column(name = COLUMN_MODULE_CODE_NAME) private String moduleCodeName;
	
	@Transient private String moduleAsString;
	@Transient private Boolean defined;
	@Transient private Boolean secured;
	@Transient private String status;
	
	@Override
	public Service setIdentifier(String identifier) {
		return (Service) super.setIdentifier(identifier);
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String FIELD_MODULE = "module";
	public static final String FIELD_MODULE_CODE_NAME = "moduleCodeName";
	public static final String FIELD_MODULE_AS_STRING = "moduleAsString";
	public static final String FIELD_DEFINED = "defined";
	public static final String FIELD_SECURED = "secured";
	public static final String FIELD_STATUS = "status";
	
	public static final String COLUMN_MODULE = "MODULE";
	public static final String COLUMN_MODULE_CODE_NAME = "MODULE_CODE_LIBELLE";
	
	public static final String TABLE_NAME = "VM_APP_SERVICE";	
}