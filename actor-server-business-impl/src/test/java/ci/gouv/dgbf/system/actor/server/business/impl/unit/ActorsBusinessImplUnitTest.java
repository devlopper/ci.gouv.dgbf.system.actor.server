package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.server.MetricsManager;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.ActorScopeRequestBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

public class ActorsBusinessImplUnitTest extends AbstractUnitTestMemory {
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
		return "actors";
	}
	
	@Test
	public void record_scope_notJoined() {
		Long count = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(count).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeRequestBusinessImpl.record(List.of("1"), List.of("s01"),"test",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(count+1);
	}
	
	@Test
	public void record_scope_joined_throwException() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.record(List.of("1"), List.of("s02"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void record_scope_notExist_throwException() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.record(List.of("1"), List.of("sXX"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void cancel_scope_joined_grantedNull() {
		Long count = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(count).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeRequestBusinessImpl.cancel(List.of("1"),"s02",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(count-1);
	}
	
	@Test
	public void cancel_scope_joined_grantedTrue() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.cancel(List.of("2"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void cancel_scope_joined_grantedFalse() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.cancel(List.of("3"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void cancel_scope_notExist() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.cancel(List.of("xX"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void process_scope_joined_grantedNull() {
		Long count = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(count).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeRequestBusinessImpl.process(List.of("1"),Map.of("1",Boolean.TRUE),null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(count);
	}
	
	@Test
	public void process_scope_joined_grantedTrue() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.process(List.of("2"), null,null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void process_scope_joined_grantedFalse() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.process(List.of("3"), null,null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void process_scope_notExist() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.process(List.of("xX"), null,null,"test", entityManager);
				}
			}.run();
		});
	}
}