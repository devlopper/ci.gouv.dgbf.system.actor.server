package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;
import io.swagger.annotations.ApiOperation;

import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeRepresentation.PATH)
public interface ScopeRepresentation extends RepresentationEntity<ScopeDto> {
	/*
	@GET
	@Path(PATH_GET_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les domaines d'un compte utilisateur",tags = {"get"})
	Response getByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	*/
	@GET
	@Path(PATH_GET_SECTIONS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les sections d'un compte utilisateur",tags = {"get","section"})
	Response getSectionsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les unités administratives d'un compte utilisateur",tags = {"get","unite_administrative"})
	Response getAdministrativeUnitsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	String PATH = "domaine";
	String PATH_GET_BY_ACTOR_CODE = "getByActorCode";
	String PATH_GET_SECTIONS_BY_ACTOR_CODE = "sections_par_acteur";
	String PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "unites_administratives_par_acteur";
}