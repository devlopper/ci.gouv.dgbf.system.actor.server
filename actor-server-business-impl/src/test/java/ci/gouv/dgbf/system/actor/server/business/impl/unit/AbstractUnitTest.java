package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Override
	protected void callInitialize() {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();
	}
}