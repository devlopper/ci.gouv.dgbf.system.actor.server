package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestFunctionRepresentation.PATH)
public interface RequestFunctionRepresentation extends RepresentationEntity<RequestFunctionDto> {
	
	String PATH = "requestfunction";
	
}
