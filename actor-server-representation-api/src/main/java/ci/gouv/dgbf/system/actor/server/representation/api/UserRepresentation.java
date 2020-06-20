package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.UserDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(UserRepresentation.PATH)
public interface UserRepresentation extends RepresentationEntity<UserDto> {
	
	String PATH = "user";
	
}
