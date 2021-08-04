package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.server.MetricsManager;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.ActorScopeBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class VisibilitiesBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		MetricsManager.getInstance().enable();
	}
	
	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		MetricsManager.getInstance().disable();
	}

	@Override
	protected String getPersistenceUnitName() {
		return "visibilities";
	}
	
	@Test
	public void visible_section_unexisting() {
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01"}
			, new String[] {"s02","s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01"}} }
			, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
		Long count = DynamicManyExecutor.getInstance().count(ActorScope.class);
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.visible(List.of("1"), List.of("s02"),null,"test", entityManager);
			}
		}.run();
		
		assertThat(DynamicManyExecutor.getInstance().count(ActorScope.class)).isEqualTo(count+1);
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",Boolean.TRUE},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01","s02"}
			, new String[] {"s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",Boolean.TRUE},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01","s02"}} }
			, new Object[][] { {"christian",new String[] {"s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
	}
	
	@Test
	public void visible_section_existing_notVisible() {
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01"}
			, new String[] {"s02","s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01"}} }
			, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
		Long count = DynamicManyExecutor.getInstance().count(ActorScope.class);
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.visible(List.of("1"), List.of("s03"),null,"test", entityManager);
			}
		}.run();
		
		assertThat(DynamicManyExecutor.getInstance().count(ActorScope.class)).isEqualTo(count);
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",Boolean.TRUE},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01","s03"}
			, new String[] {"s02","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",Boolean.TRUE},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01","s03"}} }
			, new Object[][] { {"christian",new String[] {"s02","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
	}
	
	@Test
	public void visible_section_existing_visible() {		
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeBusinessImpl.visible(List.of("1"), List.of("s01"),null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void visible_section_existing_visible_ignore() {		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.visible(List.of("1"), List.of("s01"),Boolean.TRUE,"test", entityManager);
			}
		}.run();
	}
	
	@Test
	public void unvisible_section_unexisting() {		
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeBusinessImpl.unvisible(List.of("1"), List.of("sXX"),null,"test", entityManager);
				}
			}.run();
		});
	}
	
	/*@Test
	public void unvisible_section_unvisible() {		
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeBusinessImpl.unvisible(List.of("1"), List.of("s03"),null,"test", entityManager);
				}
			}.run();
		});
	}*/
	
	@Test
	public void unvisible_section_unvisible_ignore() {				
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.unvisible(List.of("1"), List.of("s02"),null,"test", entityManager);
			}
		}.run();		
	}
	
	@Test
	public void unvisible_section_existing_notVisible_joinExist() {
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01"}
			, new String[] {"s02","s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01"}} }
			, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
		Long count = DynamicManyExecutor.getInstance().count(ActorScope.class);
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.unvisible(List.of("1"), List.of("s03"),null,"test", entityManager);
			}
		}.run();
		
		assertThat(DynamicManyExecutor.getInstance().count(ActorScope.class)).isEqualTo(count-1);
		assertScopes(typeCode
				, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
				, new String[] {"s01"}
				, new String[] {"s02","s03","s04","s05","s10","s11"}
				, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
				, new Object[][] { {"christian",new String[] {"s01"}} }
				, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
				,Boolean.TRUE);
	}
	
	//@Test
	public void unvisible_section_existing_notVisible_joinNotExist() {
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01"}
			, new String[] {"s02","s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01"}} }
			, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
		Long count = DynamicManyExecutor.getInstance().count(ActorScope.class);
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.unvisible(List.of("1"), List.of("s04"),null,"test", entityManager);
			}
		}.run();
		
		assertThat(DynamicManyExecutor.getInstance().count(ActorScope.class)).isEqualTo(count+1);
		assertScopes(typeCode
				, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
				, new String[] {"s01"}
				, new String[] {"s02","s03","s04","s05","s10","s11"}
				, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
				, new Object[][] { {"christian",new String[] {"s01"}} }
				, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
				,Boolean.TRUE);
	}
	
	@Test
	public void unvisible_section_existing_visible() {
		String typeCode = ScopeType.CODE_SECTION;
		assertScopes(typeCode
			, new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, new String[] {"s01"}
			, new String[] {"s02","s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",Boolean.TRUE},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",new String[] {"s01"}} }
			, new Object[][] { {"christian",new String[] {"s02","s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
		Long count = DynamicManyExecutor.getInstance().count(ActorScope.class);
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeBusinessImpl.unvisible(List.of("1"), List.of("s01"),null,"test", entityManager);
			}
		}.run();
		
		assertThat(DynamicManyExecutor.getInstance().count(ActorScope.class)).isEqualTo(count-1);
		assertScopes(typeCode
			, new Object[][] {{"s01",null},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}
			, null
			, new String[] {"s01","s02","s03","s04","s05","s10","s11"}
			, new Object[][] { {"christian",new Object[][] {{"s01",null},{"s02",null},{"s03",null},{"s04",null},{"s05",null},{"s10",null},{"s11",null}}} }
			, new Object[][] { {"christian",null} }
			, new Object[][] { {"christian",new String[] {"s01","s02","s03","s04","s05","s10","s11"}} }
			,Boolean.TRUE);
	}
}