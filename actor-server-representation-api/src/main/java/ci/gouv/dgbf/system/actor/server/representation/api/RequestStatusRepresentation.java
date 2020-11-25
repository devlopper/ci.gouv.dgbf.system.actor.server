package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestStatusDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestStatusRepresentation.PATH)
public interface RequestStatusRepresentation extends RepresentationEntity<RequestStatusDto> {
	
	String PATH = "requeststatus";
	
}
