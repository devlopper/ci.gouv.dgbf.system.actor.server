package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

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
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
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

	public static final String OPERATION_GET_VISIBLES_SCOPES = "obtenir_domaines_visibles";
	@GET
	@Path(OPERATION_GET_VISIBLES_SCOPES)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines visibles d'un acteur",operationId = "obtenir_domaines_visibles")
	@APIResponses(value = {
			@APIResponse(description = "Domaines visibles obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines visibles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getScopes(
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
	
	public static final String OPERATION_GET_PROFILES_CODES = "obtenir_profiles_codes";
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
}