package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.representation.server.OpenAPI;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorScopeRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfileTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeTypeRepresentation;

@Path(ScopeOpenAPI.PATH)
@Tag(name = "Domaine")
public interface ScopeOpenAPI extends OpenAPI {

	String PATH = "open/domaine";
	
	String OPERATION_GET_TYPES = "obtenir-types";
	@GET
	@Path(OPERATION_GET_TYPES)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les types de domaines",operationId = OPERATION_GET_TYPES)
	@APIResponses(value = {
			@APIResponse(description = "Types de domaines obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des types de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getTypes(
			@Parameter(description = ScopeTypeRepresentation.DESCRIPTION_REQUESTABLE,example = ScopeTypeRepresentation.EXAMPLE_REQUESTABLE,name = ScopeTypeRepresentation.PARAMETER_REQUESTABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeTypeRepresentation.PARAMETER_REQUESTABLE) Boolean requestable
			
			,@Parameter(description = ScopeTypeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeTypeRepresentation.EXAMPLE_PAGEABLE,name = ScopeTypeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ProfileTypeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeTypeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeTypeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeTypeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeTypeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeTypeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeTypeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeTypeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeTypeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_GET = "obtenir";
	@GET
	@Path(OPERATION_GET)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines",operationId = OPERATION_GET)
	@APIResponses(value = {
			@APIResponse(description = "Domaines obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response get(
			@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_CATEGORIE_BUDGET,ScopeType.CODE_CATEGORIE_ACTIVITE,ScopeType.CODE_AB,ScopeType.CODE_SECTION,ScopeType.CODE_UA
					,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_VISIBLE,example = ScopeRepresentation.EXAMPLE_VISIBLE,name = ScopeRepresentation.PARAMETER_VISIBLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_VISIBLE) Boolean visible
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeRepresentation.EXAMPLE_PAGEABLE,name = ScopeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_GET_BY_ACTOR = "obtenir-par-acteur";
	@GET
	@Path(OPERATION_GET_BY_ACTOR)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines d'un acteur",operationId = OPERATION_GET_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Domaines obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_CATEGORIE_BUDGET,ScopeType.CODE_CATEGORIE_ACTIVITE,ScopeType.CODE_AB,ScopeType.CODE_SECTION,ScopeType.CODE_UA
					,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_VISIBLE,example = ScopeRepresentation.EXAMPLE_VISIBLE,name = ScopeRepresentation.PARAMETER_VISIBLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_VISIBLE) Boolean visible
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeRepresentation.EXAMPLE_PAGEABLE,name = ScopeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_GET_VISIBLES_BY_ACTOR = "obtenir-visibles-par-acteur";
	@GET
	@Path(OPERATION_GET_VISIBLES_BY_ACTOR)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines visibles d'un acteur",operationId = OPERATION_GET_VISIBLES_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Domaines visibles obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines visibles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getVisiblesByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_CATEGORIE_BUDGET,ScopeType.CODE_CATEGORIE_ACTIVITE,ScopeType.CODE_AB,ScopeType.CODE_SECTION,ScopeType.CODE_UA
					,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeRepresentation.EXAMPLE_PAGEABLE,name = ScopeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	/* Requests */
	
	String OPERATION_GET_REQUESTS_BY_ACTOR = "obtenir-demandes-par-acteur";
	@GET
	@Path(OPERATION_GET_REQUESTS_BY_ACTOR)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les demandes de domaines d'un acteur",operationId = OPERATION_GET_REQUESTS_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Demandes de domaines obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des demandes de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getRequestsByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_PROCESSED,example = ActorScopeRequestRepresentation.EXAMPLE_PROCESSED,name = ActorScopeRequestRepresentation.PARAMETER_PROCESSED
			,allowEmptyValue = true)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_PROCESSED) Boolean processed
			
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_GRANTED,example = ActorScopeRequestRepresentation.EXAMPLE_GRANTED,name = ActorScopeRequestRepresentation.PARAMETER_GRANTED
			,allowEmptyValue = true)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_GRANTED) Boolean granted
			
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_PAGEABLE,example = ActorScopeRequestRepresentation.EXAMPLE_PAGEABLE,name = ActorScopeRequestRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ActorScopeRequestRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ActorScopeRequestRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ActorScopeRequestRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ActorScopeRequestRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_CREATE_REQUESTS_BY_ACTOR = "creer-demandes-par-acteur";
	@POST
	@Path(OPERATION_CREATE_REQUESTS_BY_ACTOR)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Créer des demandes de domaines d'un acteur",operationId = OPERATION_CREATE_REQUESTS_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Demandes domaines créées",responseCode = "201", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création des demandes de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response createRequestsByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_SCOPES_IDENTIFIERS,example = ActorScopeRequestRepresentation.EXAMPLE_SCOPES_IDENTIFIERS,name = ActorScopeRequestRepresentation.PARAMETER_SCOPES_IDENTIFIERS)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_SCOPES_IDENTIFIERS)
			List<String> requestablesIdentifiers
			);
	
	String OPERATION_DELETE_REQUESTS_BY_ACTOR = "supprimer-demandes-par-acteur";
	@POST
	@Path(OPERATION_DELETE_REQUESTS_BY_ACTOR)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Supprimer des demandes de domaines d'un acteur",operationId = OPERATION_DELETE_REQUESTS_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Demandes domaines supprimées",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la suppression des demandes de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response deleteRequestsByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_IDENTIFIERS,example = ActorScopeRequestRepresentation.EXAMPLE_IDENTIFIERS,name = ActorScopeRequestRepresentation.PARAMETER_IDENTIFIERS)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_IDENTIFIERS)
			List<String> requestsIdentifiers
			);
}