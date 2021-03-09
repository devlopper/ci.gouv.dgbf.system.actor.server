package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

public class PersistenceUnitTestTestMapping extends AbstractUnitTestValidateMapping {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "test";
	}
	
}