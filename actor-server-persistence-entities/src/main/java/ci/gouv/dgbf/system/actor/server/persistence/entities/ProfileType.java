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
@Entity @Table(name=ProfileType.TABLE_NAME)
public class ProfileType extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public ProfileType setIdentifier(String identifier) {
		return (ProfileType) super.setIdentifier(identifier);
	}
	
	@Override
	public ProfileType setCode(String code) {
		return (ProfileType) super.setCode(code);
	}
	
	@Override
	public ProfileType setName(String name) {
		return (ProfileType) super.setName(name);
	}
	
	public static final String TABLE_NAME = "TYPE_PROFILE";	
}