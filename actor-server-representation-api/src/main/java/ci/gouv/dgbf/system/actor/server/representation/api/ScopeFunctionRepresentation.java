package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeFunctionRepresentation.PATH)
public interface ScopeFunctionRepresentation extends RepresentationEntity<ScopeFunctionDto> {
	
	String PATH = "scopefunction";
	
}
