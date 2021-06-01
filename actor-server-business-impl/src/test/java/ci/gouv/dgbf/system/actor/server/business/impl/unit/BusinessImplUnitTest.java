package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.inject.Inject;
import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.annotation.H2;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.business.server.EntityReader;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.NativeQueryStringBuilder;
import org.cyk.utility.persistence.server.MetricsManager;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.AssignmentsBusinessImpl;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class BusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

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
		assertThat(instance.getCreditManagerHolder()).isNull();
		assertThat(instance.getCreditManagerAssistant()).isNull();
		instance.setCreditManagerHolder(EntityFinder.getInstance().find(ScopeFunction.class, "G100000"));
		Collection<Assignments> collection = List.of(instance);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.saveScopeFunctions(collection, entityManager);
			}
		}.run();		
		instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getCreditManagerHolder()).isNotNull();
		assertThat(instance.getCreditManagerAssistant()).isNotNull();
	}
	
	@Test
	public void applyModel_doNotOverride() {
		Assignments instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getCreditManagerHolder()).isNull();
		assertThat(instance.getCreditManagerAssistant()).isNull();
		instance.setCreditManagerHolder(EntityFinder.getInstance().find(ScopeFunction.class, "G100000"));
		Collection<Assignments> collection = List.of(instance);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.applyModel(model, filter, overridablesFieldsNames, actorCode, entityManager);
			}
		}.run();		
		instance = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(instance).isNotNull();
		assertThat(instance.getCreditManagerHolder()).isNotNull();
		assertThat(instance.getCreditManagerAssistant()).isNotNull();
	}
}