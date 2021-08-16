package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import javax.persistence.EntityManager;

import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.ScopeFunctionBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;

public class AssignmentsBusinessImplUnitTestTesting extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "test";
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_G() {
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("UA2D5E8D6889784EEBB009170777AED7F9","G1", "GC Test A SUPPRIMER","test",null, entityManager);
			}
		}.run();
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_O() {
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("USB35f5fa5c-715c-4ce1-8317-58e8c69c4f3c","O2", "ORD Test A SUPPRIMER","test",null, entityManager);
			}
		}.run();
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_C() {
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("UA2D5E8D6889784EEBB009170777AED7F9","C1", "CF Test A SUPPRIMER","test",null, entityManager);
			}
		}.run();
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_T() {
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("UA2D5E8D6889784EEBB009170777AED7F9","T1", "CPT Test A SUPPRIMER","test",null, entityManager);
			}
		}.run();
	}
}