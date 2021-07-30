package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class VisibilitiesPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "visibilities";
	}

	@Test
	public void scopeTypes(){	
		assertThat(EntityReader.getInstance().readMany(ScopeType.class, new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeTypeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))).stream().map(x -> x.getCode()).collect(Collectors.toList()))
			.containsExactly("ACTION","ACTIVITE","CATEGORIE_ACTIVITE","LOCALITE","SECTION","UA","USB");
		
		assertThat(EntityCounter.getInstance().count(ScopeType.class, new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeTypeQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC)))).isEqualTo(7);
	}
	
	@Test
	public void scopes(){
		sections();
		administrativeUnits();
		budgetSpecializationUnits();
		actions();
		activities();
	}
	
	@Test
	public void sections(){
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",Boolean.TRUE},{"s03",Boolean.TRUE},{"s04",Boolean.TRUE},{"s05",Boolean.TRUE},{"s10",Boolean.TRUE},{"s11",null}}
			, new String[] {"s01","s02","s03","s04","s05","s10"}
			, new String[] {"s11"}
			, new Object[] {"s02",new String[] {"s02"}}
			, new Object[] {"3",new String[] {"s03"}}
			, new Object[][] { {"rffim_s01",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01","s02","s03","s04","s05","s10"}} }
			, new Object[][] { {"christian",new String[] {"s11"}} }
			, new Object[][] { {"s01",new String[] {"christiann"} } });
	}
	
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
			,null,null, (Object[][])null);
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
			, null,null,null, (Object[][])null);
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
			, null,null,null, (Object[][])null);
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
			, null,null,null, (Object[][])null);
	}
}