package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.security.keycloak.ClientManager;
import org.cyk.utility.__kernel__.security.keycloak.Resource;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ServiceBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.MenuQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@ApplicationScoped
public class ServiceBusinessImpl extends AbstractBusinessEntityImpl<Service, ServicePersistence> implements ServiceBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void deleteKeycloakAuthorizations(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		deleteAllKeycloakAuthorizationPolicies(services);
		deleteAllKeycloakAuthorizationResources(services);
		LogHelper.logInfo(String.format("Keycloak authorizations has been deleted for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}
	
	@Override
	public void deriveKeycloakAuthorizations(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		//deleteKeycloakAuthorizations(services);
		deriveKeycloakAuthorizationPolicies(services);
		deriveKeycloakAuthorizationResources(services);
		deriveKeycloakAuthorizationPermissions(services);
		LogHelper.logInfo(String.format("Keycloak authorizations has been derived for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}
	
	@Override
	public void deleteAllKeycloakAuthorizationPolicies(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		ClientManager.getInstance().deleteAllAuthorizationPolicies(FieldHelper.readBusinessIdentifiersAsStrings(services));
		LogHelper.logInfo(String.format("Keycloak authorizations policies has been deleted for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}

	@Override
	public void deleteAllKeycloakAuthorizationResources(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		ClientManager.getInstance().deleteAllAuthorizationResources(FieldHelper.readBusinessIdentifiersAsStrings(services));
		LogHelper.logInfo(String.format("Keycloak authorizations resources has been deleted for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}
	
	@Override
	public void deriveKeycloakAuthorizationPolicies(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		Collection<Profile> profiles = ProfileQuerier.getInstance().read();
		if(CollectionHelper.isEmpty(profiles))
			return;
		ClientManager.getInstance().createAuthorizationPoliciesFromRolesNames(FieldHelper.readBusinessIdentifiersAsStrings(services)
				, FieldHelper.readBusinessIdentifiersAsStrings(profiles));
		LogHelper.logInfo(String.format("Keycloak authorizations policies has been derived for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}

	@Override
	public void deriveKeycloakAuthorizationResources(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		for(Service service : services) {
			Collection<Menu> menus = MenuQuerier.getInstance().readByServiceCode(service.getCode());
			if(CollectionHelper.isEmpty(menus))
				continue;
			Collection<Resource> resources = menus.stream()
				.filter(menu -> StringHelper.isNotBlank(menu.getUniformResourceIdentifier()))
				.map(menu -> Resource.build(menu.getName(),menu.getUniformResourceIdentifier()))
				.collect(Collectors.toList());
			if(CollectionHelper.isEmpty(resources))
				return;
			ClientManager.getInstance().createAuthorizationResources(List.of(service.getCode()), resources);
		}
		LogHelper.logInfo(String.format("Keycloak authorizations resources has been derived for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}

	@Override
	public void deriveKeycloakAuthorizationPermissions(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		for(Service service : services) {
			Collection<Profile> profiles = ProfileQuerier.getInstance().readByServicesIdentifiers(List.of(service.getIdentifier()));
			if(CollectionHelper.isEmpty(profiles))
				continue;
			Collection<Menu> menus = MenuQuerier.getInstance().readByServiceIdentifier(service.getIdentifier());
			if(CollectionHelper.isEmpty(menus))
				continue;
			ClientManager.getInstance().createAuthorizationPermissionFromRolesNamesAndResourcesNames(List.of(service.getCode())
					, FieldHelper.readBusinessIdentifiersAsStrings(profiles), menus.stream().map(menu -> menu.getName()).collect(Collectors.toList()));
		}
		LogHelper.logInfo(String.format("Keycloak authorizations permissions has been derived for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}
}