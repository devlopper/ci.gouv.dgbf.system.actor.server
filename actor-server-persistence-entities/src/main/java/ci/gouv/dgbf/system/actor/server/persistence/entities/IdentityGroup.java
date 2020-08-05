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
@Entity @Table(name=IdentityGroup.TABLE_NAME)
@Cacheable
public class IdentityGroup extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public IdentityGroup setIdentifier(String identifier) {
		return (IdentityGroup) super.setIdentifier(identifier);
	}
	
	@Override
	public IdentityGroup setCode(String code) {
		return (IdentityGroup) super.setCode(code);
	}
	
	@Override
	public IdentityGroup setName(String name) {
		return (IdentityGroup) super.setName(name);
	}
	
	public static final String TABLE_NAME = "GROUPE_IDENTITE";	
}