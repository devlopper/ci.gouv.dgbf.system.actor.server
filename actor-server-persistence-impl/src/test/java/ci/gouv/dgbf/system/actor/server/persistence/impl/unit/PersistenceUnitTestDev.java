package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

public class PersistenceUnitTestDev extends AbstractUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
}