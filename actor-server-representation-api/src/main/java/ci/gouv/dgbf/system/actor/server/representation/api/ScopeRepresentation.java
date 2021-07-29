package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
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
	Response getSectionsByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les unités administratives d'un compte utilisateur")
	Response getAdministrativeUnitsByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_BUDGET_SPECIALIZATION_UNITS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les unités de spécialisation du budget d'un compte utilisateur")
	Response getBudgetSpecializationUnitsByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ACTIONS_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les actions d'un compte utilisateur")
	Response getActionsByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_ACTIVITIES_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les activités d'un compte utilisateur")
	Response getActivitiesByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	@GET
	@Path(PATH_GET_LINES_BY_ACTOR_CODE)
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Obtenir les lignes d'un compte utilisateur")
	Response getLinesByActorCode(@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)String actorCode);
	
	String PATH = "domaine";
	String PATH_GET_BY_ACTOR_CODE = "getByActorCode";
	String PATH_GET_SECTIONS_BY_ACTOR_CODE = "sections_par_acteur";
	String PATH_GET_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "unites_administratives_par_acteur";
	String PATH_GET_BUDGET_SPECIALIZATION_UNITS_BY_ACTOR_CODE = "unites-specialisations-budgets-par-acteur";
	String PATH_GET_ACTIONS_BY_ACTOR_CODE = "actions-par-acteur";
	String PATH_GET_ACTIVITIES_BY_ACTOR_CODE = "activites-par-acteur";
	String PATH_GET_LINES_BY_ACTOR_CODE = "lignes-par-acteur";
	
	String TAG = "Domaines";
	
	String PARAMETER_TYPE_CODE = "code_type_domaine";
	String DESCRIPTION_TYPE_CODE = "Code du type de domaine";
	String EXAMPLE_TYPE_CODE = ScopeType.CODE_SECTION;
	
	String PARAMETER_PAGEABLE = "paginer";
	String DESCRIPTION_PAGEABLE = "Activer la pagination";
	String EXAMPLE_PAGEABLE = "true";
	
	String PARAMETER_FIRST_TUPLE_INDEX = "index";
	String DESCRIPTION_FIRST_TUPLE_INDEX = "index du premier élément";
	String EXAMPLE_FIRST_TUPLE_INDEX = "0";
	
	String PARAMETER_NUMBER_OF_TUPLES = "nombre";
	String DESCRIPTION_NUMBER_OF_TUPLES = "Nombre d'élément";
	String EXAMPLE_NUMBER_OF_TUPLES = "5";
}