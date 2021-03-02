package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.test.weld.AbstractPersistenceUnitTest;

public abstract class AbstractUnitTest extends AbstractPersistenceUnitTest {

	@Override
	protected Long count(Class<?> klass) {
		return EntityCounter.getInstance().count(klass);
	}
}
