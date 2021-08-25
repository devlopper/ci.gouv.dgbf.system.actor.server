package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.persistence.query.EntityFinder;
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
	protected String getPersistenceUnitName() {
		return "privileges";
	}
	
	@Test
	public void create() {
		String code = RandomHelper.getAlphabetic(10);
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ProfileBusinessImpl.create(code, "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(2l);
		Profile profile = EntityFinder.getInstance().find(Profile.class, code);
		assertThat(profile).isNotNull();
		assertThat(profile.getIdentifier()).isEqualTo(code);
	}
	
	@Test
	public void create_another() {
		String code = RandomHelper.getAlphabetic(10);
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ProfileBusinessImpl.create(code, "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
			}
		}.run();
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(2l);
	}
	
	@Test
	public void create_duplicate() {	
		String code = RandomHelper.getAlphabetic(10);
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ProfileBusinessImpl.create(code, "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
				}
			}.run();
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ProfileBusinessImpl.create(code, "profile",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void update() {
		String code = RandomHelper.getAlphabetic(10);
		assertThat(DynamicManyExecutor.getInstance().count(Profile.class)).isEqualTo(1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ProfileBusinessImpl.update("RPROG",code, "profile rp",ProfileType.CODE_SYSTEME,null,null,"test", entityManager);
			}
		}.run();
		Profile profile = EntityFinder.getInstance().find(Profile.class, "RPROG");
		assertThat(profile).isNotNull();
		assertThat(profile.getCode()).isEqualTo(code);
	}
}