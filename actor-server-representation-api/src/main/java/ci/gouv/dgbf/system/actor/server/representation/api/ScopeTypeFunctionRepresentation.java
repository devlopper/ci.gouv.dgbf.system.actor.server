package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeTypeFunctionRepresentation.PATH)
public interface ScopeTypeFunctionRepresentation extends RepresentationEntity<ScopeTypeFunctionDto> {
	
	String PATH = "scopetypefunction";
	
}
