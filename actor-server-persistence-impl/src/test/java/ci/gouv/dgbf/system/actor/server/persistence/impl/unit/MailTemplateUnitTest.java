package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import javax.persistence.Persistence;

import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.test.weld.AbstractPersistenceUnitTest;
import org.cyk.utility.persistence.EntityManagerFactoryGetterImpl;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.impl.FreeMarker;

public class MailTemplateUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		EntityManagerFactoryGetterImpl.ENTITY_MANAGER_FACTORY = Persistence.createEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void getRecordedMailMessage(){
		String text = FreeMarker.getRecordedMailMessage(new AccountRequest().setFirstName("Komenan").setLastNames("Yao Christian")
				.setCivility(new Civility().setName("Monsieur")));
		assertThat(text).contains("Monsieur Komenan Yao Christian").contains("votre demande a");
	}
	
	@Test
	public void getAcceptedMailMessage(){
		String text = FreeMarker.getAcceptedMailMessage(new Actor().setFirstName("Komenan").setLastNames("Yao Christian")
				.setCivility(new Civility().setName("Monsieur")).setUsername("u01").setPassword("123"));
		assertThat(text).contains("Monsieur Komenan Yao Christian").contains("votre demande a");
	}
	
	@Test
	public void getRejectedMailMessage(){
		String text = FreeMarker.getRejectedMailMessage(new RejectedAccountRequest().setFirstName("Komenan").setLastNames("Yao Christian").setReason("la raison")
				/*.setCivility(new Civility().setName("Monsieur"))*/);
		assertThat(text).contains("Komenan Yao Christian").contains("votre demande a");
	}
}
