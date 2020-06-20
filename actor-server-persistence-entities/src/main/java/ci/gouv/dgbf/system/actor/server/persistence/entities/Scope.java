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
@Entity @Table(name=Scope.TABLE_NAME)
public class Scope extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Scope setIdentifier(String identifier) {
		return (Scope) super.setIdentifier(identifier);
	}
	
	@Override
	public Scope setCode(String code) {
		return (Scope) super.setCode(code);
	}
	
	@Override
	public Scope setName(String name) {
		return (Scope) super.setName(name);
	}
	
	public static final String TABLE_NAME = "DOMAINE";	
}