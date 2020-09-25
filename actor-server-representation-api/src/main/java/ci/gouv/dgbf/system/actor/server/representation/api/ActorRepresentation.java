package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

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

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import io.swagger.annotations.ApiOperation;

@Path(ActorRepresentation.PATH)
public interface ActorRepresentation extends RepresentationEntity<ActorDto> {
	
	@POST
	@Path(PATH_CREATE_PRIVILEGES_FROM_FUNCTIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Créer des privilèges à partir de fonctions",tags = {TAG})
	Response createPrivilegesFromFunctions(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_CREATE_PROFILES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Créer des profiles",tags = {TAG})
	Response createProfiles(Collection<ActorDto> actors);
	
	@POST
	@Path(PATH_DELETE_PROFILES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Supprimer des profiles",tags = {TAG})
	Response deleteProfiles(Collection<ActorDto> actors);
	
	@GET
	@Path(PATH_GET_PROFILE_INFORMATIONS_BY_CODE)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Obtenir les informations de profile d'un compte utilisateur",tags = {TAG})
	Response getProfileInformationsByCode(@QueryParam(QUERY_PARAMETER_NAME_USER_NAME)String code);
	
	@POST
	@Path(PATH_IMPORT_FROM_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Importer les comptes utilisateurs à partir de keycloak pour les création",tags = {TAG})
	Response importFromKeycloak();
	
	@POST
	@Path(PATH_EXPORT_TO_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Exporter les acteurs vers keycloak",tags = {TAG})
	Response exportToKeycloak();
	
	@POST
	@Path(PATH_UPDATE_TO_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Mettre à jour les utilisateurs de keycloak",tags = {TAG})
	Response updateToKeycloak();
	
	@POST
	@Path(PATH_SEND_UPDATE_PASSWORD_EMAIL)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Envoyer un mail de mise à jour du mot de passe",tags = {TAG})
	Response sendUpdatePasswordEmail(@QueryParam(QUERY_PARAMETER_NAME_USER_NAME)String code);
	
	String PATH = "actor";
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
	
	String QUERY_PARAMETER_NAME_USER_NAME = "nom_utilisateur";
	
	String TAG = "Acteurs";
	
	static ActorRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorRepresentation.class);
	}
}