package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;
import java.util.List;

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
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;

@Path(ActorRepresentation.PATH)
@Tag(name = ActorRepresentation.TAG)
public interface ActorRepresentation extends RepresentationEntity<ActorDto> {
	
	String PATH_RECORD_REQUESTS = "recordrequests";
	@POST
	@Path(PATH_RECORD_REQUESTS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response recordRequests(
			@QueryParam("actors") List<String> actorsIdentifiers
			,@QueryParam("profiles") List<String> profilesIdentifiers
			,@QueryParam("scopes") List<String> scopesIdentifiers
			,@QueryParam("actorcode") String actorCode
			,@QueryParam("ignoreexisting") Boolean ignoreExisting);
	
	@GET
	@Path(PATH_GET_ONE_TO_BE_CREATED_BY_PUBLIC)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Instantier un acteur a être créé par public")
	ActorDto getOneToBeCreatedByPublic();
	
	@POST
	@Path(PATH_CREATE_BY_PUBLIC)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	/*@Operation(description = "Créer un acteur par public")
	@APIResponses(value = {
			@APIResponse(responseCode = "201",name = "Créé")
	})*/
	Response createByPublic(/*@Parameter(allowEmptyValue = false,name = "acteur",required = true)*/ ActorDto actor);
	
	@POST
	@Path(PATH_SAVE_PROFILE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	/*@Operation(description = "Enregistrer le profile")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",name = "Enregistré")
	})*/
	Response saveProfile(/*@Parameter(allowEmptyValue = false,name = "acteur",required = true)*/ ActorDto actor);
	
	@POST
	@Path(PATH_CREATE_PRIVILEGES_FROM_FUNCTIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Créer des privilèges à partir de fonctions")
	Response createPrivilegesFromFunctions(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_CREATE_PROFILES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Créer des profiles")
	Response createProfiles(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_DELETE_PROFILES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Supprimer des profiles")
	Response deleteProfiles(Collection<ActorDto> actors);
	
	@GET
	@Path(PATH_GET_PROFILE_INFORMATIONS_BY_CODE)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Obtenir les informations de profile d'un compte utilisateur")
	Response getProfileInformationsByCode(@QueryParam(PARAMETER_USER_NAME)String code);
	
	@POST
	@Path(PATH_IMPORT_FROM_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Importer les comptes utilisateurs à partir de keycloak pour les création")
	Response importFromKeycloak();
	
	@POST
	@Path(PATH_EXPORT_TO_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Exporter les acteurs vers keycloak")
	Response exportToKeycloak();
	
	@POST
	@Path(PATH_UPDATE_TO_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Mettre à jour les utilisateurs de keycloak")
	Response updateToKeycloak();
	
	@POST
	@Path(PATH_SEND_UPDATE_PASSWORD_EMAIL)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Envoyer un mail de mise à jour du mot de passe")
	Response sendUpdatePasswordEmail(@QueryParam(PARAMETER_USER_NAME)String code);
	
	String PATH = "actor";
	String PATH_GET_ONE_TO_BE_CREATED_BY_PUBLIC = "getonetobecreatedbypublic";
	String PATH_GET_PROFILE_INFORMATIONS_BY_CODE = "informations-profile-par-acteur";
	String PATH_IMPORT_FROM_KEYCLOAK = "importFromKeycloak";
	String PATH_EXPORT_TO_KEYCLOAK = "exportToKeycloak";
	String PATH_UPDATE_TO_KEYCLOAK = "updateToKeycloak";
	String PATH_SEND_UPDATE_PASSWORD_EMAIL = "sendUpdatePasswordEmail";
	//String PATH_SAVE_PREFERENCES = "savePreferences";
	//String PATH_SAVE_PROFILE = "saveProfile";
	String PATH_CREATE_PRIVILEGES_FROM_FUNCTIONS = "create_privileges_from_functions";
	String PATH_CREATE_PROFILES = "create_profiles";
	String PATH_DELETE_PROFILES = "delete_profiles";
	String PATH_CREATE_BY_PUBLIC = "createbypublic";
	String PATH_SAVE_PROFILE = "saveProfile";

	String PARAMETER_ACTORS = "acteurs";
	String DESCRIPTION_ACTORS = "Acteurs";
	String EXAMPLE_ACTORS = "komenan";
	
	//String QUERY_PARAMETER_NAME_USER_NAME = "nom_utilisateur";
	String PARAMETER_USER_NAME = "nom_utilisateur";
	String DESCRIPTION_USER_NAME = "Nom d'utilisateur";
	String EXAMPLE_USER_NAME = "komenan";
	
	String PARAMETER_FIRST_NAME = "nom";
	String PARAMETER_LAST_NAMES = "prenoms";
	String PARAMETER_ELECTRONIC_MAIL_ADDRESS = "email";
	
	String TAG = "Acteurs";
	
	static ActorRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorRepresentation.class);
	}
}