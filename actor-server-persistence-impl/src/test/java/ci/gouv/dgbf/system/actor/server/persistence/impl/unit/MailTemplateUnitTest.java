package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.impl.FreeMarker;

public class MailTemplateUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void getRecordedMailMessage(){
		String text = FreeMarker.getRecordedMailMessage(new AccountRequest().setFirstName("Komenan").setLastNames("Yao Christian")
				.setCivility(new Civility().setName("Monsieur")));
		assertThat(text).contains("Monsieur Komenan Yao Christian").contains("votre demande a");
	}
	
	@Test
	public void getAcceptedMailMessage(){
		String text = FreeMarker.getAcceptedMailMessage(new Actor().setFirstName("Komenan").setLastNames("Yao Christian")
				.setCivility(new Civility().setName("Monsieur")).setCode("u01").setPassword("123"));
		assertThat(text).contains("Monsieur Komenan Yao Christian").contains("votre demande a");		
	}
	
	@Test
	public void getRejectedMailMessage(){
		String text = FreeMarker.getRejectedMailMessage(new RejectedAccountRequest().setFirstName("Komenan").setLastNames("Yao Christian").setReason("la raison")
				/*.setCivility(new Civility().setName("Monsieur"))*/);
		assertThat(text).contains("Komenan Yao Christian").contains("votre demande a");
	}
}