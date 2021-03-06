package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.persistence.server.TransientFieldsProcessor;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		//ClientManager.IDENTIFIERS_EXCLUDABLE.addAll(CollectionHelper.listOf(Service.CODE_MIC_ACTEUR));
		//Initializer.initialize();
		//ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class,EntityLifeCycleListener.class
				,TransientFieldsProcessor.class);
		
		__inject__(org.cyk.utility.server.persistence.impl.ApplicationScopeLifeCycleListener.class).initialize(null);
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.class).initialize(null);
		
		//ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		
		__inject__(org.cyk.utility.template.freemarker.ApplicationScopeLifeCycleListener.class).initialize(null);
	}
	
	@Override
	public void __destroy__(Object object) {}
	
}