package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.RequestBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;

public class RequestsBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "requests";
	}
	
	@Test
	public void havingDispatchSlipSent_processAllRequest_processingDateShouldBeNotNull() {
		assertRequestDispatchSlipProcessingDateIsNull("1", Boolean.TRUE);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				RequestBusinessImpl.acceptByIdentifier("1", List.of("GDTI"), null, null, null, entityManager);
			}
		}.run();
		assertRequestDispatchSlipProcessingDateIsNull("1", Boolean.FALSE);
	}
}