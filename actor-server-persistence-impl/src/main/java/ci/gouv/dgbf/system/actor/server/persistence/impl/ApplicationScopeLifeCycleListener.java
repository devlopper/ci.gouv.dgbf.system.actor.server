package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.server.TransientFieldsProcessor;
import org.cyk.utility.persistence.server.audit.AuditReader;
import org.cyk.utility.persistence.server.hibernate.Initializer;
import org.cyk.utility.persistence.server.query.RuntimeQueryBuilder;
import org.cyk.utility.persistence.server.query.string.RuntimeQueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestScopeFunctionQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeFunctionQuerierImpl;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		//ClientManager.IDENTIFIERS_EXCLUDABLE.addAll(CollectionHelper.listOf(Service.CODE_MIC_ACTEUR));
		//Initializer.initialize();
		//ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		/*
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class,EntityLifeCycleListener.class
				,TransientFieldsProcessor.class,RuntimeQueryStringBuilder.class,RuntimeQueryBuilder.class,AuditReader.class);
		*/
		__inject__(org.cyk.utility.server.persistence.impl.ApplicationScopeLifeCycleListener.class).initialize(null);
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.class).initialize(null);
		
		//ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		
		__inject__(org.cyk.utility.template.freemarker.ApplicationScopeLifeCycleListener.class).initialize(null);
	}
	
	@Override
	public void __destroy__(Object object) {}
	
	/**/
	
	public static void initialize() {
		Initializer.initialize();
		
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class,EntityLifeCycleListener.class
				,TransientFieldsProcessor.class,RuntimeQueryStringBuilder.class,RuntimeQueryBuilder.class,AuditReader.class);
		
		QueryManager.getInstance().scan(List.of(ActorQuerier.class.getPackage()));	
		ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		AssignmentsQuerierImpl.initialize();
		ScopeFunctionQuerierImpl.initialize();
		RequestScopeFunctionQuerierImpl.initialize();
	}
}