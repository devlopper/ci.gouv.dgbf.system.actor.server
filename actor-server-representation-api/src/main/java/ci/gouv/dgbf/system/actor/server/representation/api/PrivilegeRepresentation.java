package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.server.representation.RepresentationEntity;
import org.eclipse.microprofile.openapi.annotations.Operation;

import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeDto;

@Path(PrivilegeRepresentation.PATH)
public interface PrivilegeRepresentation extends RepresentationEntity<PrivilegeDto> {
	
	@GET
	@Path(PATH_GET_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les privilèges d'un compte utilisateur")
	Response getByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_FOR_SERVICE_MANAGER_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les privilèges d'un compte utilisateur pour la gestion des services")
	Response getForServiceManagerByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	String PATH = "privilege";
	String PATH_GET_BY_ACTOR_CODE = "privileges-par-acteur";
	String PATH_GET_FOR_SERVICE_MANAGER_BY_ACTOR_CODE = "privileges-par-acteur-pour-gestion-services";
	
	String TAG = "Privilèges";
}