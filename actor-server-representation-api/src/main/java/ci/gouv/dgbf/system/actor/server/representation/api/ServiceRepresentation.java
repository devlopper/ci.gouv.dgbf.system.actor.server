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
import org.eclipse.microprofile.openapi.annotations.Operation;

import ci.gouv.dgbf.system.actor.server.representation.entities.ServiceDto;

@Path(ServiceRepresentation.PATH)
public interface ServiceRepresentation extends RepresentationEntity<ServiceDto> {
	
	@POST
	@Path(PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_POLICIES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Supprimer les règles d'autorisation de keycloak")
	Response deleteAllKeycloakAuthorizationPolicies(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_CREATE_KEYCLOAK_AUTHORIZATION_POLICIES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver les règles d'autorisation de keycloak")
	Response deriveKeycloakAuthorizationPolicies(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_RESOURCES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Supprimer les ressources autorisables de keycloak")
	Response deleteAllKeycloakAuthorizationResources(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_CREATE_KEYCLOAK_AUTHORIZATION_RESOURCES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver les ressources autorisables de keycloak")
	Response deriveKeycloakAuthorizationResources(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_KEYCLOAK_AUTHORIZATION_PERMISSIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver les permissions d'autorisation de keycloak")
	Response deriveKeycloakAuthorizationPermissions(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver les autorisations de keycloak")
	Response deriveKeycloakAuthorizations(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DELETE_KEYCLOAK_AUTHORIZATIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Supprimer les autorisations de keycloak")
	Response deleteKeycloakAuthorizations(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver les autorisations de keycloak à partir de zéro")
	Response deriveKeycloakAuthorizationsFromScratch(Collection<ServiceDto> services);
	
	@POST
	@Path(PATH_DERIVE_ALL_KEYCLOAK_AUTHORIZATIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver toutes les autorisations de keycloak")
	Response deriveAllKeycloakAuthorizations();
	
	@POST
	@Path(PATH_DERIVE_ALL_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Dériver toutes les autorisations de keycloak à partir de zéro")
	Response deriveAllKeycloakAuthorizationsFromScratch();
	
	String PATH = "service";
	String PATH_CREATE_KEYCLOAK_AUTHORIZATION_POLICIES = "deriveKeycloakAuthorizationPolicies";
	String PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_POLICIES = "deleteAllKeycloakAuthorizationPolicies";
	String PATH_CREATE_KEYCLOAK_AUTHORIZATION_RESOURCES = "deriveKeycloakAuthorizationResources";
	String PATH_DELETE_ALL_KEYCLOAK_AUTHORIZATION_RESOURCES = "deleteAllKeycloakAuthorizationResources";
	String PATH_DERIVE_KEYCLOAK_AUTHORIZATION_PERMISSIONS = "deriveKeycloakAuthorizationPermissions";
	String PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS = "deriveKeycloakAuthorizations";
	String PATH_DELETE_KEYCLOAK_AUTHORIZATIONS = "deleteKeycloakAuthorizations";
	String PATH_DERIVE_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH = "deriveKeycloakAuthorizationsFromScratch";
	String PATH_DERIVE_ALL_KEYCLOAK_AUTHORIZATIONS = "deriveAllKeycloakAuthorizations";
	String PATH_DERIVE_ALL_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH = "deriveAllKeycloakAuthorizationsFromScratch";
	
	String TAG = PrivilegeRepresentation.TAG;
	
	static ServiceRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ServiceRepresentation.class);
	}
}