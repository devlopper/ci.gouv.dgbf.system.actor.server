package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileTypeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ProfileTypeRepresentation.PATH)
public interface ProfileTypeRepresentation extends RepresentationEntity<ProfileTypeDto> {
	
	String PATH = "profiletype";
	
}
