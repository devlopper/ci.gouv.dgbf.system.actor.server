package ci.gouv.dgbf.system.actor.server.persistence.entities;

public class PersistenceEntitiesUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
}