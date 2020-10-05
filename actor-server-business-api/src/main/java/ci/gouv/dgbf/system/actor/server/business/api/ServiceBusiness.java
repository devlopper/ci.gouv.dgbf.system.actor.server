package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

public interface ServiceBusiness extends BusinessEntity<Service> {

	String DELETE_ALL_KEYCLOAK_AUTHORIZATION_POLICIES = "deleteAllKeycloakAuthorizationPolicies";
	Integer deleteAllKeycloakAuthorizationPolicies(Collection<Service> services);
	
	String DELETE_ALL_KEYCLOAK_AUTHORIZATION_RESOURCES = "deleteAllKeycloakAuthorizationResources";
	Integer deleteAllKeycloakAuthorizationResources(Collection<Service> services);
	
	String DELETE_KEYCLOAK_AUTHORIZATIONS = "deleteKeycloakAuthorizations";
	void deleteKeycloakAuthorizations(Collection<Service> services);
	
	/**
	 * Permissions are automatically deleted whenever the related resource or the related permission is deleted. So this must not be called.
	 */
	
	//String DELETE_ALL_KEYCLOAK_AUTHORIZATION_PERMISSIONS = "deleteAllKeycloakAuthorizationPermissions";
	//void deleteAllKeycloakAuthorizationPermissions(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATION_POLICIES = "deriveKeycloakAuthorizationPolicies";
	Integer deriveKeycloakAuthorizationPolicies(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATION_RESOURCES = "deriveKeycloakAuthorizationResources";
	Integer deriveKeycloakAuthorizationResources(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATION_PERMISSIONS = "deriveKeycloakAuthorizationPermissions";
	Integer deriveKeycloakAuthorizationPermissions(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATIONS = "deriveKeycloakAuthorizations";
	void deriveKeycloakAuthorizations(Collection<Service> services);
	
	String DERIVE_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH = "deriveKeycloakAuthorizationsFromScratch";
	void deriveKeycloakAuthorizationsFromScratch(Collection<Service> services);
	
	String DERIVE_ALL_KEYCLOAK_AUTHORIZATIONS = "deriveAllKeycloakAuthorizations";
	void deriveAllKeycloakAuthorizations();
	
	String DERIVE_ALL_KEYCLOAK_AUTHORIZATIONS_FROM_SCRATCH = "deriveAllKeycloakAuthorizationsFromScratch";
	void deriveAllKeycloakAuthorizationsFromScratch();
}