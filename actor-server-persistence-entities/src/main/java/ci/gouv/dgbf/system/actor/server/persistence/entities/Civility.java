package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Civility.TABLE_NAME)
@Cacheable
public class Civility extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Civility setIdentifier(String identifier) {
		return (Civility) super.setIdentifier(identifier);
	}
	
	@Override
	public Civility setCode(String code) {
		return (Civility) super.setCode(code);
	}
	
	@Override
	public Civility setName(String name) {
		return (Civility) super.setName(name);
	}
	
	public static final String TABLE_NAME = "CIVILITE";	
}