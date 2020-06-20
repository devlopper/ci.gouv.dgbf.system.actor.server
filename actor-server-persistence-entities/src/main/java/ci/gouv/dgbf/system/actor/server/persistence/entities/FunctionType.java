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
@Entity @Table(name=FunctionType.TABLE_NAME)
public class FunctionType extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public FunctionType setIdentifier(String identifier) {
		return (FunctionType) super.setIdentifier(identifier);
	}
	
	@Override
	public FunctionType setCode(String code) {
		return (FunctionType) super.setCode(code);
	}
	
	@Override
	public FunctionType setName(String name) {
		return (FunctionType) super.setName(name);
	}
	
	public static final String TABLE_NAME = "TYPE_FONCTION";	
}