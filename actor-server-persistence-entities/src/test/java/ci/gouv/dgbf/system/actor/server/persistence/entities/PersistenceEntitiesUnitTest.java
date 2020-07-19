package ci.gouv.dgbf.system.actor.server.persistence.entities;

import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

public class PersistenceEntitiesUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void run(){
		
	}
	
	@Test
	public void rejectedAccountRequest_create(){
		//System.out.println("PersistenceEntitiesUnitTest.rejectedAccountRequest_create()");
		//assertThat(EntityCounter.getInstance().count(RejectedAccountRequest.class)).isEqualTo(0l);
		//EntityCreator.getInstance().createOneInTransaction(new RejectedAccountRequest());
		//assertThat(EntityCounter.getInstance().count(RejectedAccountRequest.class)).isEqualTo(1l);
	}
	
}