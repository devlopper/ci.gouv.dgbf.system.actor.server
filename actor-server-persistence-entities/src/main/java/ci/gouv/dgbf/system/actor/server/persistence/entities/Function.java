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
@Entity @Table(name=Function.TABLE_NAME)
public class Function extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Function setIdentifier(String identifier) {
		return (Function) super.setIdentifier(identifier);
	}
	
	@Override
	public Function setCode(String code) {
		return (Function) super.setCode(code);
	}
	
	@Override
	public Function setName(String name) {
		return (Function) super.setName(name);
	}
	
	public static final String TABLE_NAME = "FONCTION";	
}