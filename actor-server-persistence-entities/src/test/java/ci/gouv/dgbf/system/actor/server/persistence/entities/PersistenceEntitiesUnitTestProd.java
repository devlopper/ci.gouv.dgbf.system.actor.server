package ci.gouv.dgbf.system.actor.server.persistence.entities;

public class PersistenceEntitiesUnitTestProd extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "prod";
	}
}