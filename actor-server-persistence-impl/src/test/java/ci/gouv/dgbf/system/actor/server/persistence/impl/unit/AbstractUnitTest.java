package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.cyk.utility.__kernel__.klass.PersistableClassesGetter;
import org.cyk.utility.persistence.query.EntityCounter;
import org.junit.jupiter.api.Test;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Test
	public void mapping(){
		PersistableClassesGetter.getInstance().get().stream().forEach(klass -> {
			EntityCounter.getInstance().count(klass);
		});
	}
	
}
