package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.junit.jupiter.api.Test;

public class ActorPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void readDynamicOne(){		
		assertReadDynamicOne("christian", "Monsieur", "Komenan", "Yao Christian", "Fonctionnaire", "kycdev@gmail.com", "DTI Direction des traitements informatiques"
				, "102 Sénat", "Chef de service", new String[] {"P00","P02"});
	}
	
}