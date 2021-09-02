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

import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.representation.server.OpenAPI;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfileRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfileTypeRepresentation;

@Path(ProfileOpenAPI.PATH)
@Tag(name = "Profile")
public interface ProfileOpenAPI extends OpenAPI {

	String PATH = "open/profile";
	
	String OPERATION_GET_TYPES = "obtenir-types";
	@GET
	@Path(OPERATION_GET_TYPES)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les types de profiles",operationId = OPERATION_GET_TYPES)
	@APIResponses(value = {
			@APIResponse(description = "Types de profiles obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des types de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getTypes(
			@Parameter(description = ProfileTypeRepresentation.DESCRIPTION_PAGEABLE,example = ProfileTypeRepresentation.EXAMPLE_PAGEABLE,name = ProfileTypeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ProfileTypeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ProfileTypeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ProfileTypeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ProfileTypeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ProfileTypeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ProfileTypeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ProfileTypeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ProfileTypeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ProfileTypeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_GET = "obtenir";
	@GET
	@Path(OPERATION_GET)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les profiles",operationId = OPERATION_GET)
	@APIResponses(value = {
			@APIResponse(description = "Profiles obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response get(
			@Parameter(description = ProfileRepresentation.DESCRIPTION_TYPE_IDENTIFIER,example = ProfileRepresentation.EXAMPLE_TYPE_IDENTIFIER,name = ProfileRepresentation.PARAMETER_TYPE_IDENTIFIER
			,allowEmptyValue = true)
			@QueryParam(ProfileRepresentation.PARAMETER_TYPE_IDENTIFIER) String typeIdentifier
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_REQUESTABLE,example = ProfileRepresentation.EXAMPLE_REQUESTABLE,name = ProfileRepresentation.PARAMETER_REQUESTABLE
			,allowEmptyValue = true)
			@QueryParam(ProfileRepresentation.PARAMETER_REQUESTABLE) Boolean requestable
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_PAGEABLE,example = ProfileRepresentation.EXAMPLE_PAGEABLE,name = ProfileRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ProfileRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ProfileRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ProfileRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ProfileRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ProfileRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ProfileRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ProfileRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_GET_BY_ACTOR = "obtenir-par-acteur";
	@GET
	@Path(OPERATION_GET_BY_ACTOR)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les profiles d'un acteur",operationId = OPERATION_GET_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Profiles obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_TYPE_CODE,example = ProfileRepresentation.EXAMPLE_TYPE_CODE,name = ProfileRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = true,schema = @Schema(enumeration = {ConstantEmpty.STRING,ProfileType.CODE_SYSTEME,ProfileType.CODE_UTILISATEUR}))
			@QueryParam(ProfileRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_PAGEABLE,example = ProfileRepresentation.EXAMPLE_PAGEABLE,name = ProfileRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ProfileRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ProfileRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ProfileRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ProfileRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ProfileRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ProfileRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ProfileRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ProfileRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	/* Requests */
	
	String OPERATION_GET_REQUESTS_BY_ACTOR = "obtenir-demandes-par-acteur";
	@GET
	@Path(OPERATION_GET_REQUESTS_BY_ACTOR)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les demandes de profiles d'un acteur",operationId = OPERATION_GET_REQUESTS_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Demandes de profiles obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des demandes de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getRequestsByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_PROCESSED,example = ActorProfileRequestRepresentation.EXAMPLE_PROCESSED,name = ActorProfileRequestRepresentation.PARAMETER_PROCESSED
			,allowEmptyValue = true)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_PROCESSED) Boolean processed
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_GRANTED,example = ActorProfileRequestRepresentation.EXAMPLE_GRANTED,name = ActorProfileRequestRepresentation.PARAMETER_GRANTED
			,allowEmptyValue = true)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_GRANTED) Boolean granted
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_PAGEABLE,example = ActorProfileRequestRepresentation.EXAMPLE_PAGEABLE,name = ActorProfileRequestRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ActorProfileRequestRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ActorProfileRequestRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ActorProfileRequestRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ActorProfileRequestRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	String OPERATION_CREATE_REQUESTS_BY_ACTOR = "creer-demandes-par-acteur";
	@POST
	@Path(OPERATION_CREATE_REQUESTS_BY_ACTOR)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Créer des demandes de profiles d'un acteur",operationId = OPERATION_CREATE_REQUESTS_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Demandes profiles créées",responseCode = "201", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création des demandes de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response createRequestsByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_PROFILES_IDENTIFIERS,example = ActorProfileRequestRepresentation.EXAMPLE_PROFILES_IDENTIFIERS,name = ActorProfileRequestRepresentation.PARAMETER_PROFILES_IDENTIFIERS)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_PROFILES_IDENTIFIERS)
			List<String> requestablesIdentifiers
			);
	
	String OPERATION_DELETE_REQUESTS_BY_ACTOR = "supprimer-demandes-par-acteur";
	@POST
	@Path(OPERATION_DELETE_REQUESTS_BY_ACTOR)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Supprimer des demandes de profiles d'un acteur",operationId = OPERATION_DELETE_REQUESTS_BY_ACTOR)
	@APIResponses(value = {
			@APIResponse(description = "Demandes profiles supprimées",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la suppression des demandes de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response deleteRequestsByActor(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_IDENTIFIERS,example = ActorProfileRequestRepresentation.EXAMPLE_IDENTIFIERS,name = ActorProfileRequestRepresentation.PARAMETER_IDENTIFIERS)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_IDENTIFIERS)
			List<String> requestsIdentifiers
			);
}