package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

public interface ServiceBusiness extends BusinessEntity<Service> {

	String DELETE_ALL_KEYCLOAK_AUTHORIZATION_POLICIES = "deleteAllKeycloakAuthorizationPolicies";
	void deleteAllKeycloakAuthorizationPolicies(Collection<Service> services);
	
	String DELETE_ALL_KEYCLOAK_AUTHORIZATION_RESOURCES = "deleteAllKeycloakAuthorizationResources";
	void deleteAllKeycloakAuthorizationResources(Collection<Service> services);
	
	String DELETE_KEYCLOAK_AUTHORIZATIONS = "deleteKeycloakAuthorizations";
	void deleteKeycloakAuthorizations(Collection<Service> services);
	
	/**
	 * Permissions are automatically deleted whenever the related resource or the related permission is deleted. So this must not be called.
	 */
	
	//String DELETE_ALL_KEYCLOAK_AUTHORIZATION_PERMISSIONS = "deleteAllKeycloakAuthorizationPermissions";
	//void deleteAllKeycloakAuthorizationPermissions(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATION_POLICIES = "deriveKeycloakAuthorizationPolicies";
	void deriveKeycloakAuthorizationPolicies(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATION_RESOURCES = "deriveKeycloakAuthorizationResources";
	void deriveKeycloakAuthorizationResources(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATION_PERMISSIONS = "deriveKeycloakAuthorizationPermissions";
	void deriveKeycloakAuthorizationPermissions(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATIONS = "deriveKeycloakAuthorizations";
	void deriveKeycloakAuthorizations(Collection<Service> services);
}