package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=PrivilegeType.TABLE_NAME)
public class PrivilegeType extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_ORDER_NUMBER) private Byte orderNumber;
	
	@Override
	public PrivilegeType setIdentifier(String identifier) {
		return (PrivilegeType) super.setIdentifier(identifier);
	}
	
	@Override
	public PrivilegeType setCode(String code) {
		return (PrivilegeType) super.setCode(code);
	}
	
	@Override
	public PrivilegeType setName(String name) {
		return (PrivilegeType) super.setName(name);
	}
	
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	
	public static final String COLUMN_ORDER_NUMBER = "numero_ordre";
	
	public static final String TABLE_NAME = "TYPE_PRIVILEGE";	
}