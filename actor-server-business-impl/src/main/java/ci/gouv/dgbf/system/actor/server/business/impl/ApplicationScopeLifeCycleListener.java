package ci.gouv.dgbf.system.actor.server.business.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.business.Validator;
import org.cyk.utility.business.server.EntitySaver;
import org.cyk.utility.server.business.AbstractApplicationScopeLifeCycleListenerImplementation;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListenerImplementation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public void __initialize__(Object object) {
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.class).initialize(null);
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class,EntitySaver.class,Validator.class);
	}
	
	@Override
	public void __destroy__(Object object) {}

}
