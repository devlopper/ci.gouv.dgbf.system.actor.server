package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.klass.PersistableClassesGetter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

public abstract class AbstractPersistenceApiUnitTestValidate extends org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		//ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@SuppressWarnings("unchecked")
	@Test
	public void run(){
		assertCountIsNotNull((Collection<Class<?>>) PersistableClassesGetter.COLLECTION.get());
		
	}
}