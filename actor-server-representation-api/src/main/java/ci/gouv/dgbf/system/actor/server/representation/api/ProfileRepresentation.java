package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ProfileRepresentation.PATH)
public interface ProfileRepresentation extends RepresentationEntity<ProfileDto> {
	
	String PATH = "profile";
	
}
