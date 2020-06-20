package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActorRepresentation.PATH)
public interface ActorRepresentation extends RepresentationEntity<ActorDto> {
	
	String PATH = "user";
	
}
