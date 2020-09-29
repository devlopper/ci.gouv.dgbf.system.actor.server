package ci.gouv.dgbf.system.actor.server.persistence.api;

public class PersistenceApiUnitTestDevKeycloak extends AbstractPersistenceApiUnitTestValidateKeycloak {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}

}