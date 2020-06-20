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
@Entity @Table(name=Profile.TABLE_NAME)
public class Profile extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Profile setIdentifier(String identifier) {
		return (Profile) super.setIdentifier(identifier);
	}
	
	@Override
	public Profile setCode(String code) {
		return (Profile) super.setCode(code);
	}
	
	@Override
	public Profile setName(String name) {
		return (Profile) super.setName(name);
	}
	
	
	public static final String TABLE_NAME = "PROFILE";	
}