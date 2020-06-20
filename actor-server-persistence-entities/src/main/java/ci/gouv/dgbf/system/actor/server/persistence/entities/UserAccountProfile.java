package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=UserAccountProfile.TABLE_NAME)
public class UserAccountProfile extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public UserAccountProfile setIdentifier(String identifier) {
		return (UserAccountProfile) super.setIdentifier(identifier);
	}
	
	public static final String TABLE_NAME = "COMPTE_UTILISATEUR_PROFILE";	
}