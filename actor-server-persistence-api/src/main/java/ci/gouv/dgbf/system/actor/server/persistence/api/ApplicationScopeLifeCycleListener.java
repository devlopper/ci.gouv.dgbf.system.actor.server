package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryResultMapper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.UserQuerier;

@ApplicationScoped
public class ApplicationScopeLifeCycleListener extends AbstractApplicationScopeLifeCycleListener implements Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void __initialize__(Object object) {
		__inject__(ci.gouv.dgbf.system.actor.server.persistence.entities.ApplicationScopeLifeCycleListener.class).initialize(null);
	}
	
	@Override
	public void __destroy__(Object object) {}	

	public static void initialize() {
		ci.gouv.dgbf.system.actor.server.persistence.entities.ApplicationScopeLifeCycleListener.initialize();
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class,QueryResultMapper.class, EntityReader.class,EntityCounter.class);
		
		QueryHelper.scan(List.of(UserQuerier.class.getPackage()));	
	}
}