package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

public class PersistenceApiUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void resourceByActivityQuerier_readByActivitiesCodesByBudgetaryActVersionCode_value(){
		assertThat("")
		.isEqualTo("");
	}
	
	@Override
	protected void createData() {
		
	}
}