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
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;

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
	
	public static final String OPERATION_GET_ELECTRONIC_MAIL_ADDRESS = "creer";
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
	
}