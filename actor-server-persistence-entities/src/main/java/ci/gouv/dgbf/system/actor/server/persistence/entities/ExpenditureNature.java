package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ExpenditureNature.TABLE_NAME)
public class ExpenditureNature extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public ExpenditureNature setIdentifier(String identifier) {
		return (ExpenditureNature) super.setIdentifier(identifier);
	}
	
	@Override
	public ExpenditureNature setCode(String code) {
		return (ExpenditureNature) super.setCode(code);
	}
	
	@Override
	public ExpenditureNature setName(String name) {
		return (ExpenditureNature) super.setName(name);
	}
	
	public static final String TABLE_NAME = "VM_APP_NATURE_DEPENSE";	
}