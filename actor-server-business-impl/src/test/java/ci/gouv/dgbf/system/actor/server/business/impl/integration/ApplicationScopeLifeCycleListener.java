package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener {

	public static Boolean INTEGRATION = Boolean.TRUE;
	
	@Override
	public void __initialize__(Object object) {
		if(Boolean.TRUE.equals(INTEGRATION))
			ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	public void __destroy__(Object object) {
		
	}
}