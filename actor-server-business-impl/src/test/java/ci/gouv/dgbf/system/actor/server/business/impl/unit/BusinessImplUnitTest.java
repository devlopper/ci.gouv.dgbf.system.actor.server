package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.server.MetricsManager;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.AssignmentsBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class BusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		MetricsManager.getInstance().enable();
		AssignmentsBusinessImpl.EXPORT = Boolean.FALSE;
	}
	
	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		MetricsManager.getInstance().disable();
	}

	@Test
	public void saveScopeFunctions() {
		Assignments instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getFinancialControllerHolder()).isNull();
		assertThat(instance.getFinancialControllerAssistant()).isNull();
		instance.setFinancialControllerHolder(EntityFinder.getInstance().find(ScopeFunction.class, "C300000"));
		Collection<Assignments> collection = List.of(instance);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.saveScopeFunctions(collection, entityManager);
			}
		}.run();		
		instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getFinancialControllerHolder()).isNotNull();
		assertThat(instance.getFinancialControllerAssistant()).isNotNull();
	}
	
	@Test
	public void applyModel_doNotOverride() {
		Assignments instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getFinancialControllerHolder()).isNull();
		assertThat(instance.getFinancialControllerAssistant()).isNull();
		Assignments model = new Assignments();
		model.setFinancialControllerHolder(EntityFinder.getInstance().find(ScopeFunction.class, "C300000"));
		Filter filter = new Filter();
		filter.addField(AssignmentsQuerier.PARAMETER_NAME_SECTION_CODE,"327");
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.applyModel(model, filter, null, null, entityManager);
			}
		}.run();
		instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getFinancialControllerHolder()).isNotNull();
		assertThat(instance.getFinancialControllerAssistant()).isNotNull();
	}
}