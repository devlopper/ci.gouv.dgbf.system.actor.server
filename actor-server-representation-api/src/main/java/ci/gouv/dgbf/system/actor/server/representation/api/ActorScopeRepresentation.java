package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActorScopeRepresentation.PATH)
public interface ActorScopeRepresentation extends RepresentationEntity<ActorScopeDto> {
	
	String PATH = "actorscope";
	
}
