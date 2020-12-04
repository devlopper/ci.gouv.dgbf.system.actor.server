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
import org.eclipse.microprofile.openapi.annotations.Operation;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;

@Path(ActorScopeRepresentation.PATH)
public interface ActorScopeRepresentation extends RepresentationEntity<ActorScopeDto> {
	
	@POST
	@Path(PATH_CREATE_BY_ACTORS_CODES_BY_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Créer des visibilités")
	Response createByActorsByScopes(Collection<ActorDto> actors,Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_CREATE_BY_ACTORS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Créer des visibilités")
	Response createByActors(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_DELETE_BY_ACTORS_CODES_BY_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Supprimer des visibilités")
	Response deleteByActorsByScopes(Collection<ActorDto> actors,Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_DELETE_BY_ACTORS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Supprimer des visibilités")
	Response deleteByActors(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_CREATE_BY_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "createByScopes")
	Response createByScopes(Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_DELETE_BY_SCOPES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "deleteByScopes")
	Response deleteByScopes(Collection<ScopeDto> scopes);
	
	static ActorScopeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorScopeRepresentation.class);
	}
	
	String PATH = "actorscope";
	String PATH_CREATE_BY_ACTORS_CODES_BY_SCOPES_CODES = "createByActorsCodesByScopesCodes";
	String PATH_CREATE_BY_ACTORS = "createByActors";
	String PATH_CREATE_BY_SCOPES_CODES = "createByScopes";
	String PATH_DELETE_BY_ACTORS_CODES_BY_SCOPES_CODES = "deleteByActorsCodesByScopesCodes";
	String PATH_DELETE_BY_ACTORS = "deleteByActors";
	String PATH_DELETE_BY_SCOPES = "deleteByScopes";
}