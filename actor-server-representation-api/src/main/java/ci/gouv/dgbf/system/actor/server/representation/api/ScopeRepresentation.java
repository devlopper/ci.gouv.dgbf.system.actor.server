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
	@ApiOperation(value = "Obtenir les sections d'un compte utilisateur",tags = {TAG_VISIBILITIES})
	Response getSectionsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les unités administratives d'un compte utilisateur",tags = {TAG_VISIBILITIES})
	Response getAdministrativeUnitsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_BUDGET_SPECIALIZATION_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les unités de spécialisation du budget d'un compte utilisateur",tags = {TAG_VISIBILITIES})
	Response getBudgetSpecializationUnitsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ACTIONS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les actions d'un compte utilisateur",tags = {TAG_VISIBILITIES})
	Response getActionsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ACTIVITIES_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les activités d'un compte utilisateur",tags = {TAG_VISIBILITIES})
	Response getActivitiesByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_LINES_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@ApiOperation(value = "Obtenir les lignes d'un compte utilisateur",tags = {TAG_VISIBILITIES})
	Response getLinesByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	String PATH = "domaine";
	String PATH_GET_BY_ACTOR_CODE = "getByActorCode";
	String PATH_GET_SECTIONS_BY_ACTOR_CODE = "sections_par_acteur";
	String PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "unites_administratives_par_acteur";
	String PATH_GET_BUDGET_SPECIALIZATION_UNITS_BY_ACTOR_CODE = "unites-specialisations-budgets-par-acteur";
	String PATH_GET_ACTIONS_BY_ACTOR_CODE = "actions-par-acteur";
	String PATH_GET_ACTIVITIES_BY_ACTOR_CODE = "activites-par-acteur";
	String PATH_GET_LINES_BY_ACTOR_CODE = "lignes-par-acteur";
	
	String TAG_VISIBILITIES = "Visibilités";
}