package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(FunctionRepresentation.PATH)
public interface FunctionRepresentation extends RepresentationEntity<FunctionDto> {
	
	String PATH = "function";
	
}
