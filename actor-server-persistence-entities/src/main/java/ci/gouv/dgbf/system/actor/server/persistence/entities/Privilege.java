package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Privilege.TABLE_NAME)
public class Privilege extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private PrivilegeType type;
	
	@Override
	public Privilege setIdentifier(String identifier) {
		return (Privilege) super.setIdentifier(identifier);
	}
	
	@Override
	public Privilege setCode(String code) {
		return (Privilege) super.setCode(code);
	}
	
	@Override
	public Privilege setName(String name) {
		return (Privilege) super.setName(name);
	}
	
	public Privilege setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(PrivilegeType.class, identifier));
		return this;
	}
	
	public static final String FIELD_TYPE = "type";
	
	public static final String COLUMN_TYPE = "type";
	
	public static final String TABLE_NAME = "PRIVILEGE";	
}