package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.AbstractApplicationScopeLifeCycleListener;
import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.__kernel__.persistence.query.CountQueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfilePrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;

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
		DependencyInjection.setQualifierClassTo(ci.gouv.dgbf.system.actor.server.annotation.System.class/*,QueryResultMapper.class*/, EntityReader.class,EntityCounter.class
				,CountQueryIdentifierGetter.class);
		
		FunctionQuerier.initialize();
		FunctionTypeQuerier.initialize();
		
		ScopeQuerier.initialize();
		ScopeTypeQuerier.initialize();
		
		PrivilegeQuerier.initialize();
		PrivilegeTypeQuerier.initialize();
		
		ProfileQuerier.initialize();
		ProfileTypeQuerier.initialize();
		ProfilePrivilegeQuerier.initialize();
		ProfileFunctionQuerier.initialize();
		
		ActorQuerier.initialize();
		ActorScopeQuerier.initialize();
		
		QueryHelper.scan(List.of(ActorQuerier.class.getPackage()));	
	}
}