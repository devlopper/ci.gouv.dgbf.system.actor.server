package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.cyk.utility.persistence.server.hibernate.Initializer;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		Initializer.initialize();
		ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
	}
		
}
