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

import ci.gouv.dgbf.system.actor.server.representation.entities.ServiceDto;
import io.swagger.annotations.ApiOperation;

@Path(ServiceRepresentation.PATH)
public interface ServiceRepresentation extends RepresentationEntity<ServiceDto> {
	
	@POST
	@Path(PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_POLICIES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Supprimer les règles d'autorisation de keycloak",tags = {TAG})
	Response deleteAllKeycloakAuthorizationPolicies(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_CREATE_KEYCLOAK_AUTHORIZATION_POLICIES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver les règles d'autorisation de keycloak",tags = {TAG})
	Response deriveKeycloakAuthorizationPolicies(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_RESOURCES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Supprimer les ressources autorisables de keycloak",tags = {TAG})
	Response deleteAllKeycloakAuthorizationResources(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_CREATE_KEYCLOAK_AUTHORIZATION_RESOURCES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver les ressources autorisables de keycloak",tags = {TAG})
	Response deriveKeycloakAuthorizationResources(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_KEYCLOAK_AUTHORIZATION_PERMISSIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver les permissions d'autorisation de keycloak",tags = {TAG})
	Response deriveKeycloakAuthorizationPermissions(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver les autorisations de keycloak",tags = {TAG})
	Response deriveKeycloakAuthorizations(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DELETE_KEYCLOAK_AUTHORIZATIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Supprimer les autorisations de keycloak",tags = {TAG})
	Response deleteKeycloakAuthorizations(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver les autorisations de keycloak à partir de zéro",tags = {TAG})
	Response deriveKeycloakAuthorizationsFromScratch(Collection<ServiceDto> services);
	
	String PATH = "service";
	String PATH_CREATE_KEYCLOAK_AUTHORIZATION_POLICIES = "deriveKeycloakAuthorizationPolicies";
	String PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_POLICIES = "deleteAllKeycloakAuthorizationPolicies";
	String PATH_CREATE_KEYCLOAK_AUTHORIZATION_RESOURCES = "deriveKeycloakAuthorizationResources";
	String PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_RESOURCES = "deleteAllKeycloakAuthorizationResources";
	String PATH_DERIVE_KEYCLOAK_AUTHORIZATION_PERMISSIONS = "deriveKeycloakAuthorizationPermissions";
	String PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS = "deriveKeycloakAuthorizations";
	String PATH_DELETE_KEYCLOAK_AUTHORIZATIONS = "deleteKeycloakAuthorizations";
	String PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH = "deriveKeycloakAuthorizationsFromScratch";
	
	String TAG = PrivilegeRepresentation.TAG;
	
	static ServiceRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ServiceRepresentation.class);
	}
}