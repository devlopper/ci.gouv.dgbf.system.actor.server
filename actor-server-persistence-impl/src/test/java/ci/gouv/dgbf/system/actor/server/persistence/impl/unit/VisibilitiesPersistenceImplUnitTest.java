package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.junit.jupiter.api.Test;

public class VisibilitiesPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "visibilities";
	}

	@Test
	public void visible_section(){		
		assertVisibleSectionsCodes("christian",null,null,new String[] {"s01","s02","s03","s04","s05","s10"});
		assertVisibleSectionsCodes("christian","0",null,new String[] {"s01","s02","s03","s04","s05","s10"});
		assertVisibleSectionsCodes("christian","1",null,new String[] {"s01","s10"});
	}
	
	@Test
	public void visible_administrativeUnit(){		
		assertVisibleAdministrativeUnitsCodes("christian", null,null,new String[] {"ua01"});
	}
	
	@Test
	public void visible_budgetSpecializationUnit(){		
		assertVisibleBudgetSpecializationUnitsCodes("christian", null,null,new String[] {"usb01"});
	}
	
	@Test
	public void visible_action(){		
		assertVisibleActionsCodes("christian", null,null,new String[] {"a01"});
	}
	
	@Test
	public void visible_activity(){		
		assertVisibleActivitiesCodes("christian", null,null,new String[] {"ac01","ac02"});
		assertVisibleActivitiesCodes("christian", null,"2",new String[] {"ac02"});
	}
}