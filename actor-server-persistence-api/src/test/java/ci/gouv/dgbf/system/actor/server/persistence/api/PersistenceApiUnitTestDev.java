package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public class PersistenceApiUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeQuerier_readVisibleSessionsByActorCode(){
		//assertThat(ScopeQuerier.getInstance().readVisibleSessionsByActorCode("1")).isNull();
		assertThat(ScopeQuerier.getInstance().readVisibleSessionsByActorCode("komenanyc@yahoo.fr").stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("102","321","327");
	}
	
	@Test
	public void scopeQuerier_countVisibleSessionsByActorCode(){
		//assertThat(ScopeQuerier.getInstance().readVisibleSessionsByActorCode("1")).isNull();
		assertThat(ScopeQuerier.getInstance().countVisibleSessionsByActorCode("komenanyc@yahoo.fr")).isEqualTo(3l);
	}
}