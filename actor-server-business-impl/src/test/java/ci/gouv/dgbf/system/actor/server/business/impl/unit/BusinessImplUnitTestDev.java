package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import org.cyk.utility.persistence.server.MetricsManager;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;

public class BusinessImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		MetricsManager.getInstance().enable();
	}
	
	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		MetricsManager.getInstance().disable();
	}

	@Test
	public void assignments_export() {
		for(Integer index = 0; index < 100; index = index + 1) {
			__inject__(AssignmentsBusiness.class).export("test");
		}
	}
}