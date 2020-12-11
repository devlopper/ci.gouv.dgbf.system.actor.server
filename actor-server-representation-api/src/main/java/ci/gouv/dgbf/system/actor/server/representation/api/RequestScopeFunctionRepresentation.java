package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestScopeFunctionRepresentation.PATH)
public interface RequestScopeFunctionRepresentation extends RepresentationEntity<RequestScopeFunctionDto> {
	
	String PATH = "requestscopefunction";
	
}
