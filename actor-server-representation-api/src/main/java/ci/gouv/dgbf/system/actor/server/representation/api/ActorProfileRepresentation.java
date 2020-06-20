package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActorProfileRepresentation.PATH)
public interface ActorProfileRepresentation extends RepresentationEntity<ActorProfileDto> {
	
	String PATH = "useraccountprofile";
	
}
