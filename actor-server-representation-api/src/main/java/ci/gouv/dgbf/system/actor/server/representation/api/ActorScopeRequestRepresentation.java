package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.representation.SpecificRepresentation;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeRequestDto;

@Path(ActorScopeRequestRepresentation.PATH)
public interface ActorScopeRequestRepresentation extends SpecificRepresentation<ActorScopeRequestDto> {
	
	String PATH = "actorscoperequest";
	
	String PATH_RECORD = "record";
	@POST
	@Path(PATH_RECORD)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response record(@QueryParam("actors") List<String> actorsIdentifiers,@QueryParam("scopes") List<String> scopesIdentifiers,@QueryParam("actorcode") String actorCode
			,@QueryParam("ignoreexisting") Boolean ignoreExisting);
	
	String PATH_CANCEL = "cancel";
	@POST
	@Path(PATH_CANCEL)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response cancel(@QueryParam("identifiers") List<String> identifiers,@QueryParam("actorcode") String actorCode,@QueryParam("ignoreexisting") Boolean ignoreExisting);

	String PATH_PROCESS = "process";
	@POST
	@Path(PATH_PROCESS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response process(List<ActorScopeRequestDto> actorScopeRequestDtos,@QueryParam("actorcode") String actorCode);
	
	static ActorScopeRequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorScopeRequestRepresentation.class);
	}
}