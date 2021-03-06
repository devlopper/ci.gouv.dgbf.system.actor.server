package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		__inject__(org.cyk.utility.server.persistence.impl.ApplicationScopeLifeCycleListener.class).initialize(null);
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.class).initialize(null);
		ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	public void __destroy__(Object object) {}
	
}
