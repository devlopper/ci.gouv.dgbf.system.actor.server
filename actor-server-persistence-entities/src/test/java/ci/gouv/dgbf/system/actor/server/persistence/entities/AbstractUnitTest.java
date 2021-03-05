package ci.gouv.dgbf.system.actor.server.persistence.entities;

import org.cyk.utility.__kernel__.klass.PersistableClassesGetter;
import org.cyk.utility.persistence.query.EntityCounter;
import org.junit.jupiter.api.Test;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Test
	public void mapping(){
		PersistableClassesGetter.getInstance().get().stream().forEach(klass -> {
			EntityCounter.getInstance().count(klass);
		});
	}
}