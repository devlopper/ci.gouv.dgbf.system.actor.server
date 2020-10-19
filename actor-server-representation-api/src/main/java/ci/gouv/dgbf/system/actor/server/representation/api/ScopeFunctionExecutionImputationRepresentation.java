package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionExecutionImputationDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeFunctionExecutionImputationRepresentation.PATH)
public interface ScopeFunctionExecutionImputationRepresentation extends RepresentationEntity<ScopeFunctionExecutionImputationDto> {
	
	String PATH = "scopefunctionexecutionimputation";
	
}
