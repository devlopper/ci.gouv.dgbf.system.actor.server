package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeDto;

@Path(PrivilegeRepresentation.PATH)
public interface PrivilegeRepresentation extends RepresentationEntity<PrivilegeDto> {
	
	@POST
	@Path("refresh")
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Initialiser les affectations")
	Response refresh();
	
	@GET
	@Path(PATH_GET_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les privilèges d'un compte utilisateur")
	Response getByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_FOR_SERVICE_MANAGER_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les privilèges d'un compte utilisateur pour la gestion des services")
	Response getForServiceManagerByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	String PATH = "privilege";
	String PATH_GET_BY_ACTOR_CODE = "privileges-par-acteur";
	String PATH_GET_FOR_SERVICE_MANAGER_BY_ACTOR_CODE = "privileges-par-acteur-pour-gestion-services";
	
	String TAG = "Privilèges";
	
	static PrivilegeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(PrivilegeRepresentation.class);
	}
}