package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentityGroupDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentityGroupRepresentation.PATH)
public interface IdentityGroupRepresentation extends RepresentationEntity<IdentityGroupDto> {
	
	String PATH = "identitygroup";
	
}
