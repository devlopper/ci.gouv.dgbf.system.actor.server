package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestTypeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestTypeRepresentation.PATH)
public interface RequestTypeRepresentation extends RepresentationEntity<RequestTypeDto> {
	
	String PATH = "requesttype";
	
}
