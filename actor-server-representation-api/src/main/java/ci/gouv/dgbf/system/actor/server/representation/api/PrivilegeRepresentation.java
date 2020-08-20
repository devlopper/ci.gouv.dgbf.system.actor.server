package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeDto;
import io.swagger.annotations.ApiOperation;

import org.cyk.utility.server.representation.RepresentationEntity;

@Path(PrivilegeRepresentation.PATH)
public interface PrivilegeRepresentation extends RepresentationEntity<PrivilegeDto> {
	
	@GET
	@Path(PATH_GET_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les privilèges d'un compte utilisateur",tags = {TAG_PRIVILEGES})
	Response getByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_FOR_SERVICE_MANAGER_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les privilèges d'un compte utilisateur pour la gestion des services",tags = {TAG_PRIVILEGES})
	Response getForServiceManagerByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	String PATH = "privilege";
	String PATH_GET_BY_ACTOR_CODE = "privileges-par-acteur";
	String PATH_GET_FOR_SERVICE_MANAGER_BY_ACTOR_CODE = "privileges-par-acteur-pour-gestion-services";
	
	String TAG_PRIVILEGES = "Privilèges";
}