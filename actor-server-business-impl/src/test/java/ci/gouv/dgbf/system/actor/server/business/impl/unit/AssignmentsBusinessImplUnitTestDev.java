package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import javax.persistence.EntityManager;

import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.ScopeFunctionBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;

public class AssignmentsBusinessImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_G1() {
		//assertThatCountIsEqualTo(ScopeFunction.class, 23136l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("UA5f705816-4384-4459-82d5-45c2cf625a9f","G1", "Gestionnaire de crédits Collège Moderne D'Appimandoum A SUPPRIMER","test",null, entityManager);
			}
		}.run();
		//assertThatCountIsEqualTo(ScopeFunction.class, 23138l);
		//assertScopeFunction("G100001", "Gestionnaire de crédits Collège Moderne D'Appimandoum A SUPPRIMER");
		//assertScopeFunction("A1000010", "Assistant Gestionnaire de crédits Collège Moderne D'Appimandoum A SUPPRIMER");
	}
}