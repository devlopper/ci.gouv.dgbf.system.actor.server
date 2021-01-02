package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AuthorizingOfficerServiceDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AuthorizingOfficerServiceRepresentation.PATH)
public interface AuthorizingOfficerServiceRepresentation extends RepresentationEntity<AuthorizingOfficerServiceDto> {
	
	String PATH = "authorizingofficerservice";
	
}
