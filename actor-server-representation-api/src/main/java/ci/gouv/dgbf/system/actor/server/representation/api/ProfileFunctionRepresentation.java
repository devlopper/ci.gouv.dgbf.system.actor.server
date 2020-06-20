package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ProfileFunctionRepresentation.PATH)
public interface ProfileFunctionRepresentation extends RepresentationEntity<ProfileFunctionDto> {
	
	String PATH = "profilefunction";
	
}
