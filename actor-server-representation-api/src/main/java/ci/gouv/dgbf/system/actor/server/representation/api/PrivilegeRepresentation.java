package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(PrivilegeRepresentation.PATH)
public interface PrivilegeRepresentation extends RepresentationEntity<PrivilegeDto> {
	
	String PATH = "privilege";
	
}
