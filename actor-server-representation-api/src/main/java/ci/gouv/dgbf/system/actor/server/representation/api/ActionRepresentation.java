package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActionRepresentation.PATH)
public interface ActionRepresentation extends RepresentationEntity<ActionDto> {
	
	String PATH = "action";
	
}
