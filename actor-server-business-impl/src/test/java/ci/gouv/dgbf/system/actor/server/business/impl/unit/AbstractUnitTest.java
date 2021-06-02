package ci.gouv.dgbf.system.actor.server.business.impl.unit;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Override
	protected void callInitialize() {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
	}
}