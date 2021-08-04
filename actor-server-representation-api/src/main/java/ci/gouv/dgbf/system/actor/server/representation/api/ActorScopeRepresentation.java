package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;

@Path(ActorScopeRepresentation.PATH)
public interface ActorScopeRepresentation extends RepresentationEntity<ActorScopeDto> {
	
	@POST
	@Path(PATH_VISIBLE)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	Response visible(@QueryParam("actors") List<String> actorsIdentifiers,@QueryParam("scopes") List<String> scopesIdentifiers
			,@QueryParam("ignoreexisting") Boolean ignoreExisting,@QueryParam("code") String actorCode);
	
	@POST
	@Path(PATH_UNVISIBLE)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	Response unvisible(@QueryParam("actors") List<String> actorsIdentifiers,@QueryParam("scopes") List<String> scopesIdentifiers
			,@QueryParam("ignoreexisting") Boolean ignoreExisting,@QueryParam("code") String actorCode);
	
	@POST
	@Path(PATH_CREATE_BY_ACTORS_CODES_BY_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	//@Operation(description = "Créer des visibilités")
	Response createByActorsByScopes(Collection<ActorDto> actors,Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_CREATE_BY_ACTORS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	//@Operation(description = "Créer des visibilités")
	Response createByActors(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_DELETE_BY_ACTORS_CODES_BY_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	//@Operation(description = "Supprimer des visibilités")
	Response deleteByActorsByScopes(Collection<ActorDto> actors,Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_DELETE_BY_ACTORS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	//@Operation(description = "Supprimer des visibilités")
	Response deleteByActors(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_CREATE_BY_SCOPES_CODES)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	//@Operation(description = "createByScopes")
	Response createByScopes(Collection<ScopeDto> scopes);
	
	@POST
	@Path(PATH_DELETE_BY_SCOPES)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON })
	//@Operation(description = "deleteByScopes")
	Response deleteByScopes(Collection<ScopeDto> scopes);
	
	static ActorScopeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorScopeRepresentation.class);
	}
	
	String PATH = "actorscope";
	String PATH_VISIBLE = "visible";
	String PATH_UNVISIBLE = "unvisible";
	
	String PATH_CREATE_BY_ACTORS_CODES_BY_SCOPES_CODES = "createByActorsCodesByScopesCodes";
	String PATH_CREATE_BY_ACTORS = "createByActors";
	String PATH_CREATE_BY_SCOPES_CODES = "createByScopes";
	String PATH_DELETE_BY_ACTORS_CODES_BY_SCOPES_CODES = "deleteByActorsCodesByScopesCodes";
	String PATH_DELETE_BY_ACTORS = "deleteByActors";
	String PATH_DELETE_BY_SCOPES = "deleteByScopes";
}