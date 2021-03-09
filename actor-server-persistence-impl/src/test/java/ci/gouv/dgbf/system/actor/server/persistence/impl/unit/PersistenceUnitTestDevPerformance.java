package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

public class PersistenceUnitTestDevPerformance extends AbstractUnitTestDevTestProdPerformance {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev_performance";
	}
	
}