package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeTypeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(PrivilegeTypeRepresentation.PATH)
public interface PrivilegeTypeRepresentation extends RepresentationEntity<PrivilegeTypeDto> {
	
	String PATH = "privilegetype";
	
}
