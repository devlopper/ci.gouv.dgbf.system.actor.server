package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ProfilePrivilegeDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ProfileDto profile;
	private PrivilegeDto privilege;

	@Override
	public ProfilePrivilegeDto set__deletable__(Boolean __deletable__) {
		return (ProfilePrivilegeDto) super.set__deletable__(__deletable__);
	}
}