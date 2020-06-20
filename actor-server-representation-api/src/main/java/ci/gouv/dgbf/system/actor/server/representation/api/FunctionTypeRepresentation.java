package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionTypeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(FunctionTypeRepresentation.PATH)
public interface FunctionTypeRepresentation extends RepresentationEntity<FunctionTypeDto> {
	
	String PATH = "functiontype";
	
}
