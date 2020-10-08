package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ExecutionImputationRepresentation.PATH)
public interface ExecutionImputationRepresentation extends RepresentationEntity<ExecutionImputationDto> {
	
	String PATH = "executionimputation";
	
}
