package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeRepresentation.PATH)
public interface ScopeRepresentation extends RepresentationEntity<ScopeDto> {
	
	String PATH = "scope";
	
}
