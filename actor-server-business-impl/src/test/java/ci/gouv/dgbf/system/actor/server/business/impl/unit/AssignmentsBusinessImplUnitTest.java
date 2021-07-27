package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.AssignmentsBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class AssignmentsBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	/*@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		MetricsManager.getInstance().enable();
	}
	
	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		MetricsManager.getInstance().disable();
	}*/

	@Override
	protected String getPersistenceUnitName() {
		return "assignments";
	}
	
	@Test
	public void deriveValuesByIdentifiers() {
		assertThatCountIsEqualTo(Assignments.class, 1l);
		Assignments assignments = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(assignments.getCreditManagerHolder()).as("gc").isNull();
		assertThat(assignments.getCreditManagerAssistant()).as("agc").isNull();
		assertThat(assignments.getAuthorizingOfficerHolder()).as("ord").isNull();
		assertThat(assignments.getAuthorizingOfficerAssistant()).as("aord").isNull();
		assertThat(assignments.getFinancialControllerHolder()).as("cf").isNull();
		assertThat(assignments.getFinancialControllerAssistant()).as("acf").isNull();
		assertThat(assignments.getAccountingHolder()).as("cpt").isNull();
		assertThat(assignments.getAccountingAssistant()).as("acpt").isNull();
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.deriveValuesByIdentifiers(List.of("1"), Boolean.TRUE, Boolean.TRUE, null, "test", entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Assignments.class, 1l);
		assignments = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(assignments.getCreditManagerHolder()).as("gc").isNotNull();
		assertThat(assignments.getCreditManagerAssistant()).as("agc").isNotNull();
		assertThat(assignments.getAuthorizingOfficerHolder()).as("ord").isNotNull();
		assertThat(assignments.getAuthorizingOfficerAssistant()).as("aord").isNotNull();
		assertThat(assignments.getFinancialControllerHolder()).as("cf").isNotNull();
		assertThat(assignments.getFinancialControllerAssistant()).as("acf").isNotNull();
		//assertThat(assignments.getAccountingHolder()).as("cpt").isNotNull();
		//assertThat(assignments.getAccountingAssistant()).as("acpt").isNotNull();
	}
}