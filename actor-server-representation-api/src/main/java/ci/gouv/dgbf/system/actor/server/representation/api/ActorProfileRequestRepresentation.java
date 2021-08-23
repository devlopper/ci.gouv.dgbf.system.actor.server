package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileRequestDto;

@Path(ActorProfileRequestRepresentation.PATH)
public interface ActorProfileRequestRepresentation extends AbstractActorRequestRepresentation<ActorProfileRequestDto> {
	
	String PATH = "actorprofilerequest";
	
	static ActorProfileRequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorProfileRequestRepresentation.class);
	}
}
