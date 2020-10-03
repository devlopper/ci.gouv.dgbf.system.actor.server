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
		deriveKeycloakAuthorizationPolicies(services);
		deriveKeycloakAuthorizationResources(services);
		deriveKeycloakAuthorizationPermissions(services);
		LogHelper.logInfo(String.format("Keycloak authorizations has been derived for services : %s", services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
	}
	
	@Override
	public void deriveKeycloakAuthorizationsFromScratch(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		deleteKeycloakAuthorizations(services);
		deriveKeycloakAuthorizations(services);
	}
	
	@Override
	public Integer deleteAllKeycloakAuthorizationPolicies(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		Integer count = ClientManager.getInstance().deleteAllAuthorizationPolicies(FieldHelper.readBusinessIdentifiersAsStrings(services));
		LogHelper.logInfo(String.format("Keycloak authorizations policies (%s) has been deleted for services : %s", count,services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
		return count;
	}

	@Override
	public Integer deleteAllKeycloakAuthorizationResources(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		Integer count = ClientManager.getInstance().deleteAllAuthorizationResources(FieldHelper.readBusinessIdentifiersAsStrings(services));
		LogHelper.logInfo(String.format("Keycloak authorizations resources (%s) has been deleted for services : %s", count,services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
		return count;
	}
	
	@Override
	public Integer deriveKeycloakAuthorizationPolicies(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		Collection<Profile> profiles = ProfileQuerier.getInstance().readByServicesIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(services));
		if(CollectionHelper.isEmpty(profiles))
			return null;
		Integer count = ClientManager.getInstance().createAuthorizationPoliciesFromRolesNames(FieldHelper.readBusinessIdentifiersAsStrings(services)
				, List.of(Profile.CODE_UTILISATEUR));
		count = count + ClientManager.getInstance().createAuthorizationPoliciesFromRolesNames(FieldHelper.readBusinessIdentifiersAsStrings(services)
				, FieldHelper.readBusinessIdentifiersAsStrings(profiles));
		LogHelper.logInfo(String.format("Keycloak authorizations policies (%s) has been derived for services : %s", count,services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
		return count;
	}

	@Override
	public Integer deriveKeycloakAuthorizationResources(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		Integer count = 0;
		for(Service service : services) {
			Collection<Menu> menus = MenuQuerier.getInstance().readByServiceCode(service.getCode());
			if(CollectionHelper.isEmpty(menus))
				continue;
			List<Resource> resources = menus.stream()
				.filter(menu -> StringHelper.isNotBlank(menu.getUniformResourceIdentifier()))
				.map(menu -> Resource.build(menu.getName(),menu.getUniformResourceIdentifier()))
				.collect(Collectors.toList());
			if(CollectionHelper.isEmpty(resources))
				return null;
			resources.add(new Resource().setName(getRootName(service)).setUniformResourceIdentifiers(List.of(getRootUrl(service))));
			count = count + ClientManager.getInstance().createAuthorizationResources(List.of(service.getCode()), resources);
		}
		LogHelper.logInfo(String.format("Keycloak authorizations resources (%s) has been derived for services : %s", count,services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
		return count;
	}

	@Override
	public Integer deriveKeycloakAuthorizationPermissions(Collection<Service> services) {
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("services", services);
		Integer count = 0;
		for(Service service : services) {
			Collection<Profile> profiles = ProfileQuerier.getInstance().readByServicesIdentifiers(List.of(service.getIdentifier()));
			if(CollectionHelper.isEmpty(profiles))
				continue;
			Collection<Menu> menus = MenuQuerier.getInstance().readByServiceIdentifier(service.getIdentifier());
			if(CollectionHelper.isEmpty(menus))
				continue;
			Collection<String> servicesCodes = List.of(service.getCode());
			ClientManager.getInstance().createAuthorizationPermissionFromRolesNamesAndResourcesNames(servicesCodes,List.of(Profile.CODE_UTILISATEUR),List.of(getRootName(service)));
			count++;
			for(Menu menu : menus) {
				profiles = ProfileQuerier.getInstance().readByMenus(menu);
				if(CollectionHelper.isEmpty(profiles))
					continue;
				count = count + ClientManager.getInstance().createAuthorizationPermissionFromRolesNamesAndResourcesNames(servicesCodes
						,FieldHelper.readBusinessIdentifiersAsStrings(profiles),List.of(menu.getName()));
			}			
		}
		LogHelper.logInfo(String.format("Keycloak authorizations permissions (%s) has been derived for services : %s", count,services.stream().map(x->x.getCode()).collect(Collectors.toList()))
				, getClass());
		return count;
	}
	
	private static String getRootName(Service service) {
		return StringHelper.isBlank(service.getCode()) ? Menu.__ROOT__NAME : service.getCode();
	}
	
	private static String getRootUrl(Service service) {
		return Menu.__ROOT__URL;
	}
}