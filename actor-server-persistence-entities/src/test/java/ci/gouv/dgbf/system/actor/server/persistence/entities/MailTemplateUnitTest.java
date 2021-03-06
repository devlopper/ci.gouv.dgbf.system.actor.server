package ci.gouv.dgbf.system.actor.server.persistence.entities;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.test.weld.AbstractWeldUnitTest;
import org.junit.jupiter.api.Test;

public class MailTemplateUnitTest extends AbstractWeldUnitTest {
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
