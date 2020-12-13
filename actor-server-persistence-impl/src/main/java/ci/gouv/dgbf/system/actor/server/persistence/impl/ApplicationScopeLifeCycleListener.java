package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.security.keycloak.server.ClientManager;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		ClientManager.IDENTIFIERS_EXCLUDABLE.addAll(CollectionHelper.listOf(Service.CODE_MIC_ACTEUR));
		
		__inject__(org.cyk.utility.server.persistence.impl.ApplicationScopeLifeCycleListener.class).initialize(null);
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.class).initialize(null);
		ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	public void __destroy__(Object object) {}
	
}
