package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestScopeRepresentation.PATH)
public interface RequestScopeRepresentation extends RepresentationEntity<RequestScopeDto> {
	
	String PATH = "requestscope";
	
}
