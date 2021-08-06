package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileRequestDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActorProfileRequestRepresentation.PATH)
public interface ActorProfileRequestRepresentation extends RepresentationEntity<ActorProfileRequestDto> {
	
	String PATH = "actorprofilerequest";
	
}
