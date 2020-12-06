package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;

@Path(ScopeRepresentation.PATH)
public interface ScopeRepresentation extends RepresentationEntity<ScopeDto> {
	/*
	@GET
	@Path(PATH_GET_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines d'un compte utilisateur",tags = {"get"})
	Response getByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	*/
	@GET
	@Path(PATH_GET_SECTIONS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les sections d'un compte utilisateur")
	Response getSectionsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les unités administratives d'un compte utilisateur")
	Response getAdministrativeUnitsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_BUDGET_SPECIALIZATION_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les unités de spécialisation du budget d'un compte utilisateur")
	Response getBudgetSpecializationUnitsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ACTIONS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les actions d'un compte utilisateur")
	Response getActionsByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ACTIVITIES_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les activités d'un compte utilisateur")
	Response getActivitiesByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_LINES_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les lignes d'un compte utilisateur")
	Response getLinesByActorCode(@QueryParam(ActorRepresentation.QUERY_PARAMETER_NAME_USER_NAME)String actorCode);
	
	String PATH = "domaine";
	String PATH_GET_BY_ACTOR_CODE = "getByActorCode";
	String PATH_GET_SECTIONS_BY_ACTOR_CODE = "sections_par_acteur";
	String PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "unites_administratives_par_acteur";
	String PATH_GET_BUDGET_SPECIALIZATION_UNITS_BY_ACTOR_CODE = "unites-specialisations-budgets-par-acteur";
	String PATH_GET_ACTIONS_BY_ACTOR_CODE = "actions-par-acteur";
	String PATH_GET_ACTIVITIES_BY_ACTOR_CODE = "activites-par-acteur";
	String PATH_GET_LINES_BY_ACTOR_CODE = "lignes-par-acteur";
	
	String TAG = "Domaines";
}