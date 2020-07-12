package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.klass.PersistableClassesGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public abstract class AbstractPersistenceApiUnitTestValidate extends org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@SuppressWarnings("unchecked")
	//@Test
	public void run(){
		assertCountIsNotNull((Collection<Class<?>>) PersistableClassesGetter.COLLECTION.get());	
	}
	
	@Test
	public void scopeQuerier_readWhereFilterByTypesCodes_SECTION(){
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_TYPE_CODE, "SECTION")
				.addFilterField(ScopeQuerier.PARAMETER_NAME_NAME, "Constitutionnel Conseil"));
		assertThat(scopes).isNotNull();
		assertThat(scopes.stream().map(Scope::getName).collect(Collectors.toList())).containsExactly("Conseil Constitutionnel");
	}
}