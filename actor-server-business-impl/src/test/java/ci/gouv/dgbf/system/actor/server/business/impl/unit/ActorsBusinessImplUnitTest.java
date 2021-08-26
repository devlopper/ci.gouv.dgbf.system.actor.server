package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.ActorBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.ActorProfileRequestBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.ActorScopeRequestBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

public class ActorsBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "actors";
	}
	
	@Test
	public void recordRequests_profiles() {
		Long actorProfileRequestCount = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		assertThat(actorProfileRequestCount).isEqualTo(3);
		Long actorScopeRequestCount = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(actorScopeRequestCount).isEqualTo(3);		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorBusinessImpl.recordRequests(List.of("1","2"), List.of("p01"), null,"test",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorProfileRequest.class)).isEqualTo(actorProfileRequestCount+2);
	}
	
	@Test
	public void recordRequests_scopes() {
		Long actorProfileRequestCount = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		assertThat(actorProfileRequestCount).isEqualTo(3);
		Long actorScopeRequestCount = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(actorScopeRequestCount).isEqualTo(3);		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorBusinessImpl.recordRequests(List.of("1","2"), null, List.of("s01"),"test",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(actorProfileRequestCount+2);
	}
	
	@Test
	public void recordRequests_profiles_scopes() {
		Long actorProfileRequestCount = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		assertThat(actorProfileRequestCount).isEqualTo(3);
		Long actorScopeRequestCount = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(actorScopeRequestCount).isEqualTo(3);		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorBusinessImpl.recordRequests(List.of("1","2"), List.of("p01"), List.of("s01"),"test",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorProfileRequest.class)).isEqualTo(actorProfileRequestCount+2);
		assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(actorScopeRequestCount+2);		
	}
	
	/* Scope Request*/
	
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
	public void record_scope_notJoined_many_throwException() {
		Long count = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		assertThat(count).isEqualTo(3);
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.record(List.of("1"), List.of("s01"),"test",null, entityManager);
				}
			}.run();
			
			assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(count+1);
			
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorScopeRequestBusinessImpl.record(List.of("1"), List.of("s01"),"test",null, entityManager);
				}
			}.run();
		});
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
		Long actorScopeRequestCount = DynamicManyExecutor.getInstance().count(ActorScopeRequest.class);
		Long actorScopeCount = DynamicManyExecutor.getInstance().count(ActorScope.class);
		assertThat(actorScopeRequestCount).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorScopeRequestBusinessImpl.process(List.of("1"),Map.of("1",Boolean.TRUE),null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorScopeRequest.class)).isEqualTo(actorScopeRequestCount);
		assertThat(DynamicManyExecutor.getInstance().count(ActorScope.class)).isEqualTo(actorScopeCount+1);
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
	
	/* Profile Request*/
	
	@Test
	public void record_profile_notJoined() {
		Long count = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		assertThat(count).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorProfileRequestBusinessImpl.record(List.of("1"), List.of("p01"),"test",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorProfileRequest.class)).isEqualTo(count+1);
	}
	
	@Test
	public void record_profile_notJoined_many_throwException() {
		Long count = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		assertThat(count).isEqualTo(3);
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.record(List.of("1"), List.of("p01"),"test",null, entityManager);
				}
			}.run();
			
			assertThat(DynamicManyExecutor.getInstance().count(ActorProfileRequest.class)).isEqualTo(count+1);
			
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.record(List.of("1"), List.of("p01"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void record_profile_joined_throwException() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.record(List.of("1"), List.of("p02"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void record_profile_notExist_throwException() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.record(List.of("1"), List.of("pXX"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void cancel_profile_joined_grantedNull() {
		Long count = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		assertThat(count).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorProfileRequestBusinessImpl.cancel(List.of("1"),"p02",null, entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorProfileRequest.class)).isEqualTo(count-1);
	}
	
	@Test
	public void cancel_profile_joined_grantedTrue() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.cancel(List.of("2"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void cancel_profile_joined_grantedFalse() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.cancel(List.of("3"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void cancel_profile_notExist() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.cancel(List.of("xX"),"test",null, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void process_profile_joined_grantedNull() {
		Long actorScopeRequestCount = DynamicManyExecutor.getInstance().count(ActorProfileRequest.class);
		Long actorScopeCount = DynamicManyExecutor.getInstance().count(ActorScope.class);
		assertThat(actorScopeRequestCount).isEqualTo(3);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ActorProfileRequestBusinessImpl.process(List.of("1"),Map.of("1",Boolean.TRUE),null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(ActorProfileRequest.class)).isEqualTo(actorScopeRequestCount);
		assertThat(DynamicManyExecutor.getInstance().count(ActorProfile.class)).isEqualTo(actorScopeCount+1);
	}
	
	@Test
	public void process_profile_joined_grantedTrue() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.process(List.of("2"), null,null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void process_profile_joined_grantedFalse() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.process(List.of("3"), null,null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void process_profile_notExist() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ActorProfileRequestBusinessImpl.process(List.of("xX"), null,null,"test", entityManager);
				}
			}.run();
		});
	}
}