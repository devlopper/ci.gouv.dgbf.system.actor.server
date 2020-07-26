package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActorFunctionRepresentation.PATH)
public interface ActorFunctionRepresentation extends RepresentationEntity<ActorFunctionDto> {
	
	String PATH = "actorfunction";
	
}
