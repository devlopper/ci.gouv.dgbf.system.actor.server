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

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;
import io.swagger.annotations.ApiOperation;

@Path(ActorScopeRepresentation.PATH)
public interface ActorScopeRepresentation extends RepresentationEntity<ActorScopeDto> {
	
	@POST
	@Path(PATH_DELETE_BY_ACTOR_CODE_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "deleteByActorByScopes",tags = {"delete"})
	Response deleteByActorByScopes(ActorDto actor,Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_DELETE_BY_SCOPES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "deleteByScopes",tags = {"delete"})
	Response deleteByScopes(Collection<ScopeDto> scopes);
	
	static ActorScopeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorScopeRepresentation.class);
	}
	
	String PATH = "actorscope";
	String PATH_DELETE_BY_ACTOR_CODE_SCOPES_CODES = "deleteByActorByScopes";
	String PATH_DELETE_BY_SCOPES = "deleteByScopes";
}