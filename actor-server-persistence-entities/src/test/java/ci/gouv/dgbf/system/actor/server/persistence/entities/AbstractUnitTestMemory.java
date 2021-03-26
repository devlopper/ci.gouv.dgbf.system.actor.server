package ci.gouv.dgbf.system.actor.server.persistence.entities;

public abstract class AbstractUnitTestMemory extends AbstractUnitTest {

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
}