package ci.gouv.dgbf.system.actor.server.representation.api;
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
	
	@GET
	@Path(PATH_GET_PROFILE_INFORMATIONS_BY_CODE)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Obtenir les informations de profile d'un compte utilisateur",tags = {TAG_ACTORS})
	Response getProfileInformationsByCode(@QueryParam(QUERY_PARAMETER_NAME_USER_NAME)String code);
	
	@POST
	@Path(PATH_IMPORT_FROM_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Importer les comptes utilisateurs à partir de keycloak",tags = {TAG_ACTORS})
	Response importFromKeycloak();
	
	@POST
	@Path(PATH_EXPORT_TO_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Exporter les acteurs vers keycloak",tags = {TAG_ACTORS})
	Response exportToKeycloak();
	
	@POST
	@Path(PATH_SEND_UPDATE_PASSWORD_EMAIL)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Envoyer un mail de mise à jour du mot de passe",tags = {TAG_ACTORS})
	Response sendUpdatePasswordEmail(@QueryParam(QUERY_PARAMETER_NAME_USER_NAME)String code);
	
	String PATH = "actor";
	String PATH_GET_PROFILE_INFORMATIONS_BY_CODE = "informations-profile-par-acteur";
	String PATH_IMPORT_FROM_KEYCLOAK = "importFromKeycloak";
	String PATH_EXPORT_TO_KEYCLOAK = "exportToKeycloak";
	String PATH_SEND_UPDATE_PASSWORD_EMAIL = "sendUpdatePasswordEmail";
	//String PATH_SAVE_PREFERENCES = "savePreferences";
	//String PATH_SAVE_PROFILE = "saveProfile";
	
	String QUERY_PARAMETER_NAME_USER_NAME = "nom_utilisateur";
	
	String TAG_ACTORS = "Acteurs";
	
	static ActorRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ActorRepresentation.class);
	}
}