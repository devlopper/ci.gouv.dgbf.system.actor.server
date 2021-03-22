package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener {

	@Override
	public void __initialize__(Object object) {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	public void __destroy__(Object object) {
		
	}
}