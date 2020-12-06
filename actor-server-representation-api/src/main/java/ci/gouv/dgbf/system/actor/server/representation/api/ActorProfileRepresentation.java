package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileDto;

@Path(ActorProfileRepresentation.PATH)
public interface ActorProfileRepresentation extends RepresentationEntity<ActorProfileDto> {
	
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer les profiles")
	Response save(Collection<ActorProfileDto> actorProfileDtos);
	
	static ActorProfileRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorProfileRepresentation.class);
	}
	
	String PATH = "actorprofile";
	String PATH_SAVE = "save";
}