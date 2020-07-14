package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentityDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentityRepresentation.PATH)
public interface IdentityRepresentation extends RepresentationEntity<IdentityDto> {
	
	String PATH = "identity";
	
}
