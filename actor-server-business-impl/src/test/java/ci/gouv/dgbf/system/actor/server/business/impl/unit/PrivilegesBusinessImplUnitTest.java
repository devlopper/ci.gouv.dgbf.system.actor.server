package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.MetricsManager;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.ProfileBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

public class PrivilegesBusinessImplUnitTest extends AbstractUnitTestMemory {
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
		return "privileges";
	}
	
	@Test
	public void create() {
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ProfileBusinessImpl.create("p01", "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(2l);
		Profile profile = EntityFinder.getInstance().find(Profile.class, "p01");
		assertThat(profile).isNotNull();
		assertThat(profile.getIdentifier()).isEqualTo("p01");
	}
	
	@Test
	public void create_another() {
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ProfileBusinessImpl.create("p02", "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(2l);
	}
	
	@Test
	public void create_duplicate() {		
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ProfileBusinessImpl.create("p03", "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
				}
			}.run();
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ProfileBusinessImpl.create("p03", "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void update() {
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ProfileBusinessImpl.update("RPROG","RPROG001", "profile rp",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
			}
		}.run();
		Profile profile = EntityFinder.getInstance().find(Profile.class, "RPROG");
		assertThat(profile).isNotNull();
		assertThat(profile.getCode()).isEqualTo("RPROG001");
	}
}