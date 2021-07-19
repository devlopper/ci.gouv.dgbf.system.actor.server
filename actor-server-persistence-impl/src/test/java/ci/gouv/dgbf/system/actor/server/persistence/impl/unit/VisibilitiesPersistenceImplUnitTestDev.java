package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class VisibilitiesPersistenceImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}

	//@Test
	public void scopes(){
		sections();
		//administrativeUnits();
		//budgetSpecializationUnits();
		//actions();
		//activities();
	}
	
	@Test
	public void sections(){
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"101",Boolean.TRUE},{"102",Boolean.TRUE},{"103",Boolean.TRUE},{"105",Boolean.TRUE}}
			, new String[] {"101","102"}
			, null
			, new Object[] {"32",new String[] {"327"}}
			, new Object[] {"budget",new String[] {"327"}}
			, new Object[][] { {"christian",new Object[][] {{"101",Boolean.TRUE}}} }
			, new Object[][] { {"christian",new String[] {"101"}} }
			, new Object[][] { {"christian",null} }
			);
		assertScopesCodes(ScopeType.CODE_SECTION, null, null, null, null, new String[] {"101"});
		assertScopesCodes(ScopeType.CODE_SECTION, null, null, Boolean.TRUE, null, new String[] {"101"});
		assertScopesCodes(ScopeType.CODE_SECTION, null, null, Boolean.FALSE, null, null);
	}
	/*
	@Test
	public void administrativeUnits(){
		String typeCode = ScopeType.CODE_UA;
		assertScopes(typeCode
				, new Object[][] {{"ua01",Boolean.TRUE},{"ua02",Boolean.TRUE},{"ua10",null}}
				, new String[] {"ua01","ua02"}
				, new String[] {"ua10"}
				, new Object[] {"01",new String[] {"ua01"}}
				, new Object[] {"10",new String[] {"ua10"}}
				, new Object[][] { {"gc_ua02",new Object[][] {{"ua01",null},{"ua02",Boolean.TRUE},{"ua10",null}}} }
				,null,null);
	}
	
	@Test
	public void budgetSpecializationUnits(){
		String typeCode = ScopeType.CODE_USB;
		assertScopes(typeCode
				, new Object[][] {{"usb01",Boolean.TRUE},{"usb02",Boolean.TRUE},{"usb03",Boolean.TRUE},{"usb04",null}}
				, new String[] {"usb01","usb02","usb03"}
				, new String[] {"usb04"}
				, new Object[] {"01",new String[] {"usb01"}}
				, new Object[] {"4",new String[] {"usb04"}}
				, null,null,null);
	}
	
	@Test
	public void actions(){
		String typeCode = ScopeType.CODE_ACTION;
		assertScopes(typeCode
				, new Object[][] {{"a01",Boolean.TRUE},{"a02",Boolean.TRUE},{"a03",null}}
				, new String[] {"a01","a02"}
				, new String[] {"a03"}
				, new Object[] {"01",new String[] {"a01"}}
				, new Object[] {"2",new String[] {"a02"}}
				, null,null,null);
	}
	
	@Test
	public void activities(){
		String typeCode = ScopeType.CODE_ACTIVITE;
		assertScopes(typeCode
				, new Object[][] {{"ac01",Boolean.TRUE},{"ac02",Boolean.TRUE},{"ac03",null}}
				, new String[] {"ac01","ac02"}
				, new String[] {"ac03"}
				, new Object[] {"01",new String[] {"ac01"}}
				, new Object[] {"ac",new String[] {"ac01","ac02","ac03"}}
				, null,null,null);
	}
	*/
}