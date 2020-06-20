package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfilePrivilegeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ProfilePrivilegeRepresentation.PATH)
public interface ProfilePrivilegeRepresentation extends RepresentationEntity<ProfilePrivilegeDto> {
	
	String PATH = "profileprivilege";
	
}
