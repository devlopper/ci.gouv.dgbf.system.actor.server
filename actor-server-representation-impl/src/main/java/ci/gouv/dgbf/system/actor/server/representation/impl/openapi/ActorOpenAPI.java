package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
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
import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorScopeRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeRepresentation;

@Path(ActorOpenAPI.PATH)
@Tag(name = "Acteur")
public interface ActorOpenAPI extends OpenAPI {

	public static final String PATH = "open/acteur";
	
	public static final String OPERATION_CREATE = "creer";
	@POST
	@Path(OPERATION_CREATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Créer un acteur",operationId = "creer_acteur")
	@APIResponses(value = {
			@APIResponse(description = "Acteur créé",responseCode = "201", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création de l'acteur",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})	
	public Response create(
			@Parameter(allowEmptyValue = false,description = "Nom",example = "Komenan",name = "nom",required = true)
			@FormParam("nom") String firstName
			
			,@Parameter(allowEmptyValue = false,description = "Prénoms",example = "Yao Chrstian",name = "prenoms",required = true)
			@FormParam("prenoms") String lastNames
			
			,@Parameter(description = "Email",example = "komenanyc@yahoo.fr",name = "email",required = true)
			@FormParam("email") String electronicMailAddress
			
			,@Parameter(description = "Identifiant de la civilité",example = "01",name = "civilite")
			@FormParam("civilite") String civilityIdentifier
			
			,@Parameter(description = "Identifiant du groupe",example = "02",name = "groupe")
			@FormParam("groupe") String groupIdentifier);
	
	public static final String OPERATION_GET = "obtenir";
	@GET
	@Path(OPERATION_GET)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les informations d'un acteur",operationId = "obtenir_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Informations de l'acteur obtenues", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})	
	public Response get(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = ActorRepresentation.PARAMETER_USER_NAME,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String code);
	
	public static final String OPERATION_GET_ELECTRONIC_MAIL_ADDRESS = "obtenir_adresse_electronique";
	@GET
	@Path(OPERATION_GET_ELECTRONIC_MAIL_ADDRESS)
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Obtenir l'adresse électronique d'un acteur",operationId = "obtenir_adresse_electronique")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Adresse électronique de l'acteur", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})
	public Response getElectronicMailAddress(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = ActorRepresentation.PARAMETER_USER_NAME,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String code);
	
	public static final String OPERATION_CHECK_EXISTENCE = "verifier_existence";
	@GET
	@Path(OPERATION_CHECK_EXISTENCE)
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Vérifier l'existence d'un acteur",operationId = "verifier_existence_acteur")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",description = "Acteur existe", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "400",description = "Nom d'utilisateur obligatoire", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(responseCode = "404",description = "Nom d'utilisateur inconnu", content = @Content(mediaType = MediaType.TEXT_PLAIN))
	})
	public Response checkExistense(
			@Parameter(allowEmptyValue = false,description = "Nom d'utilisateur",example = "komenan",name = ActorRepresentation.PARAMETER_USER_NAME,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String code);

	/* Scopes */
	
	public static final String OPERATION_GET_SCOPES = "obtenir-domaines";
	@GET
	@Path(OPERATION_GET_SCOPES)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines d'un acteur",operationId = "obtenir-domaines")
	@APIResponses(value = {
			@APIResponse(description = "Domaines obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getScopes(
			@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_CATEGORIE_BUDGET,ScopeType.CODE_CATEGORIE_ACTIVITE,ScopeType.CODE_AB,ScopeType.CODE_SECTION,ScopeType.CODE_UA
					,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_VISIBLE,example = ScopeRepresentation.EXAMPLE_VISIBLE,name = ScopeRepresentation.PARAMETER_VISIBLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_VISIBLE) Boolean visible
			
			,@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeRepresentation.EXAMPLE_PAGEABLE,name = ScopeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	public static final String OPERATION_GET_VISIBLES_SCOPES = "obtenir_domaines_visibles";
	@GET
	@Path(OPERATION_GET_VISIBLES_SCOPES)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines visibles d'un acteur",operationId = "obtenir_domaines_visibles")
	@APIResponses(value = {
			@APIResponse(description = "Domaines visibles obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines visibles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getVisibleScopes(
			@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_CATEGORIE_BUDGET,ScopeType.CODE_CATEGORIE_ACTIVITE,ScopeType.CODE_AB,ScopeType.CODE_SECTION,ScopeType.CODE_UA
					,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeRepresentation.EXAMPLE_PAGEABLE,name = ScopeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	public static final String OPERATION_GET_REQUEST_SCOPES = "obtenir-demandes-domaines";
	@GET
	@Path(OPERATION_GET_REQUEST_SCOPES)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les demandes de domaines d'un acteur",operationId = "obtenir-demandes-domaines")
	@APIResponses(value = {
			@APIResponse(description = "Demandes de domaines obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des demandes de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getRequestScopes(
			/*@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_SECTION,ScopeType.CODE_USB}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,*/@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
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
	
	public static final String OPERATION_CREATE_REQUEST_SCOPES = "creer-demandes-domaines";
	@POST
	@Path(OPERATION_CREATE_REQUEST_SCOPES)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Créer demandes de domaines",operationId = "creer-demandes-domaines")
	@APIResponses(value = {
			@APIResponse(description = "Demandes domaines créées",responseCode = "201", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création des demandes de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response createRequestScopes(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_SCOPES_IDENTIFIERS,example = ActorScopeRequestRepresentation.EXAMPLE_SCOPES_IDENTIFIERS,name = ActorScopeRequestRepresentation.PARAMETER_SCOPES_IDENTIFIERS)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_SCOPES_IDENTIFIERS)
			List<String> requestablesIdentifiers
			);
	
	public static final String OPERATION_DELETE_REQUEST_SCOPES = "supprimer-demandes-domaines";
	@POST
	@Path(OPERATION_DELETE_REQUEST_SCOPES)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Supprimer demandes de domaines",operationId = "supprimer-demandes-domaines")
	@APIResponses(value = {
			@APIResponse(description = "Demandes domaines supprimées",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la suppression des demandes de domaines",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response deleteRequestScopes(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_IDENTIFIERS,example = ActorScopeRequestRepresentation.EXAMPLE_IDENTIFIERS,name = ActorScopeRequestRepresentation.PARAMETER_IDENTIFIERS)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_IDENTIFIERS)
			List<String> requestsIdentifiers
			);
	
	/* Profiles */
	
	public static final String OPERATION_GET_PROFILES = "obtenir-profiles";
	@GET
	@Path(OPERATION_GET_PROFILES)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les profiles d'un acteur",operationId = "obtenir-profiles")
	@APIResponses(value = {
			@APIResponse(description = "Profiles obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getProfiles(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			);
	
	public static final String OPERATION_GET_PROFILES_CODES = "obtenir_codes_profiles";
	@GET
	@Path(OPERATION_GET_PROFILES_CODES)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les codes profiles d'un acteur",operationId = "obtenir_codes_profiles")
	@APIResponses(value = {
			@APIResponse(description = "Codes profiles obtenus",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des codes profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getProfilesCodes(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			);
	
	public static final String OPERATION_GET_REQUEST_PROFILES = "obtenir-demandes-profiles";
	@GET
	@Path(OPERATION_GET_REQUEST_PROFILES)
	@Produces({MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les demandes de profiles d'un acteur",operationId = "obtenir-demandes-profiles")
	@APIResponses(value = {
			@APIResponse(description = "Demandes de profiles obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des demandes de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getRequestProfiles(
			/*@Parameter(description = ProfileRepresentation.DESCRIPTION_TYPE_CODE,example = ProfileRepresentation.EXAMPLE_TYPE_CODE,name = ProfileRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ProfileType.CODE_SYSTEME,ProfileType.CODE_UTILISATEUR}))
			@QueryParam(ProfileRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,*/@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_PROCESSED,example = ActorProfileRequestRepresentation.EXAMPLE_PROCESSED,name = ActorProfileRequestRepresentation.PARAMETER_PROCESSED
			,allowEmptyValue = true)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_PROCESSED) Boolean processed
			
			,@Parameter(description = ActorScopeRequestRepresentation.DESCRIPTION_GRANTED,example = ActorScopeRequestRepresentation.EXAMPLE_GRANTED,name = ActorScopeRequestRepresentation.PARAMETER_GRANTED
			,allowEmptyValue = true)
			@QueryParam(ActorScopeRequestRepresentation.PARAMETER_GRANTED) Boolean granted
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_PAGEABLE,example = ActorProfileRequestRepresentation.EXAMPLE_PAGEABLE,name = ActorProfileRequestRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ActorProfileRequestRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ActorProfileRequestRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ActorProfileRequestRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ActorProfileRequestRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	public static final String OPERATION_CREATE_PROFILE_SCOPES = "creer-demandes-profiles";
	@POST
	@Path(OPERATION_CREATE_PROFILE_SCOPES)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Créer demandes de profiles",operationId = "creer-demandes-profiles")
	@APIResponses(value = {
			@APIResponse(description = "Demandes profiles créées",responseCode = "201", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création des demandes de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response createRequestProfiles(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_PROFILES_IDENTIFIERS,example = ActorProfileRequestRepresentation.EXAMPLE_PROFILES_IDENTIFIERS,name = ActorProfileRequestRepresentation.PARAMETER_PROFILES_IDENTIFIERS)
			List<String> requestablesIdentifiers
			);
	
	public static final String OPERATION_DELETE_PROFILE_SCOPES = "supprimer-demandes-profiles";
	@POST
	@Path(OPERATION_DELETE_PROFILE_SCOPES)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.TEXT_PLAIN})
	@Operation(description = "Supprimer demandes de profiles",operationId = "supprimer-demandes-profiles")
	@APIResponses(value = {
			@APIResponse(description = "Demandes profiles supprimées",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la suppression des demandes de profiles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response deleteRequestProfiles(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME)
			String actorCode
			,@Parameter(description = ActorProfileRequestRepresentation.DESCRIPTION_IDENTIFIERS,example = ActorProfileRequestRepresentation.EXAMPLE_IDENTIFIERS,name = ActorProfileRequestRepresentation.PARAMETER_IDENTIFIERS)
			@QueryParam(ActorProfileRequestRepresentation.PARAMETER_IDENTIFIERS)
			List<String> requestsIdentifiers
			);
}