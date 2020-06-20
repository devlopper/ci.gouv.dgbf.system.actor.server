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
@Entity @Table(name=Privilege.TABLE_NAME)
public class Privilege extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
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
	
	public static final String TABLE_NAME = "PRIVILEGE";	
}