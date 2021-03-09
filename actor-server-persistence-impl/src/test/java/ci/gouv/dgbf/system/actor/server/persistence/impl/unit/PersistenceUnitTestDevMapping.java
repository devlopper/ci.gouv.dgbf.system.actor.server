package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

public class PersistenceUnitTestDevMapping extends AbstractUnitTestValidateMapping {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
}