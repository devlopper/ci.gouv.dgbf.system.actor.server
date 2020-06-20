package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.UserAccountDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(UserAccountRepresentation.PATH)
public interface UserAccountRepresentation extends RepresentationEntity<UserAccountDto> {
	
	String PATH = "useraccount";
	
}
