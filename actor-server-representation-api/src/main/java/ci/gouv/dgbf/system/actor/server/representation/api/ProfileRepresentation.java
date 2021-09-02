package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileDto;

@Path(ProfileRepresentation.PATH)
public interface ProfileRepresentation extends RepresentationEntity<ProfileDto> {
	
	String PATH_CREATE = "create";
	@POST
	@Path(PATH_CREATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.APPLICATION_JSON})
	Response create(@QueryParam(Profile.FIELD_CODE) String code,@QueryParam(Profile.FIELD_NAME) String name,@QueryParam(Profile.FIELD_TYPE) String typeIdentifier
			,@QueryParam(Profile.FIELD_ORDER_NUMBER) Byte orderNumber,@QueryParam(Profile.FIELD_REQUESTABLE) Boolean requestable,@QueryParam("actor_code") String actorCode);
	
	String PATH_UPDATE = "update";
	@POST
	@Path(PATH_UPDATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.APPLICATION_JSON})
	Response update(@QueryParam(Profile.FIELD_IDENTIFIER) String identifier,@QueryParam(Profile.FIELD_CODE) String code,@QueryParam(Profile.FIELD_NAME) String name
			,@QueryParam(Profile.FIELD_TYPE) String typeIdentifier,@QueryParam(Profile.FIELD_ORDER_NUMBER) Byte orderNumber
			,@QueryParam(Profile.FIELD_REQUESTABLE) Boolean requestable,@QueryParam("actor_code") String actorCode);
	
	String PATH_SAVE = "save";
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.APPLICATION_JSON})
	Response save(@QueryParam(Profile.FIELD_IDENTIFIER) String identifier,@QueryParam(Profile.FIELD_CODE) String code,@QueryParam(Profile.FIELD_NAME) String name
			,@QueryParam(Profile.FIELD_TYPE) String typeIdentifier,@QueryParam(Profile.FIELD_ORDER_NUMBER) Byte orderNumber
			,@QueryParam(Profile.FIELD_REQUESTABLE) Boolean requestable,@QueryParam("actor_code") String actorCode);
	
	/*
	String PATH_SAVE_PRIVILEGES = "save_privileges";
	@POST
	@Path(PATH_SAVE_PRIVILEGES)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Enregistrer des privilèges")
	Response savePrivileges(Collection<ProfileDto> profiles);
	*/
	@POST
	@Path(PATH_SAVE_PRIVILEGES)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	//@Operation(description = "Enregistrer des privilèges")
	Response savePrivileges(Collection<ProfileDto> profiles);
	
	@POST
	@Path(PATH_IMPORT_FROM_KEYCLOAK_ROLES)
	@Produces({ MediaType.APPLICATION_JSON})
	//@Operation(description = "Importer les profiles à partir des roles de keycloak")
	Response importFromKeycloakRoles();
	
	@POST
	@Path(PATH_EXPORT_TO_KEYCLOAK_ROLES)
	@Produces({ MediaType.APPLICATION_JSON})
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
	
	String PARAMETER_REQUESTABLE = "demandable";
	String DESCRIPTION_REQUESTABLE = "Demandable";
	String EXAMPLE_REQUESTABLE = "true";
	
	String PARAMETER_TYPE_IDENTIFIER = "identifiant_type_profile";
	String DESCRIPTION_TYPE_IDENTIFIER = "Identifiant du type de profile";
	String EXAMPLE_TYPE_IDENTIFIER = "1";
	
	String PARAMETER_TYPE_CODE = "code_type_profile";
	String DESCRIPTION_TYPE_CODE = "Code du type de profile";
	String EXAMPLE_TYPE_CODE = ProfileType.CODE_SYSTEME;
}