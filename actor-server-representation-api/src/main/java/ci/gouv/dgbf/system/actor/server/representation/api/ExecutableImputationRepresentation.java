package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutableImputationDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ExecutableImputationRepresentation.PATH)
public interface ExecutableImputationRepresentation extends RepresentationEntity<ExecutableImputationDto> {
	
	String PATH = "executableimputation";
	
}
