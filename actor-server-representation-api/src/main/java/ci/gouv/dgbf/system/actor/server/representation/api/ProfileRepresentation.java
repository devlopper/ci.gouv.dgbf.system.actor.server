package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileDto;

@Path(ProfileRepresentation.PATH)
public interface ProfileRepresentation extends RepresentationEntity<ProfileDto> {
	
	@POST
	@Path(PATH_SAVE_PRIVILEGES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des privilèges")
	Response savePrivileges(Collection<ProfileDto> profiles);
	
	@POST
	@Path(PATH_IMPORT_FROM_KEYCLOAK_ROLES)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Importer les profiles à partir des roles de keycloak")
	Response importFromKeycloakRoles();
	
	@POST
	@Path(PATH_EXPORT_TO_KEYCLOAK_ROLES)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Exporter les profiles vers les roles de keycloak")
	Response exportToKeycloakRoles();
	
	String PATH_SAVE_PRIVILEGES = "save_privileges";
	String PATH_IMPORT_FROM_KEYCLOAK_ROLES = "importFromKeycloakRoles";
	String PATH_EXPORT_TO_KEYCLOAK_ROLES = "exportToKeycloakRoles";
	String PATH = "profile";

	String TAG = "Profiles";
	
	static ProfileRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ProfileRepresentation.class);
	}
}
