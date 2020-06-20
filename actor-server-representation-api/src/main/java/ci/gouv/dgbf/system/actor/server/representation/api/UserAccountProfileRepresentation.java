package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.UserAccountProfileDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(UserAccountProfileRepresentation.PATH)
public interface UserAccountProfileRepresentation extends RepresentationEntity<UserAccountProfileDto> {
	
	String PATH = "useraccountprofile";
	
}
