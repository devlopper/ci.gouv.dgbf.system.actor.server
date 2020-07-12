package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public class PersistenceApiUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	//@Test
	public void scopeQuerier_readVisibleSessionsByActorCode(){
		//assertThat(ScopeQuerier.getInstance().readVisibleSessionsByActorCode("1")).isNull();
		assertThat(ScopeQuerier.getInstance().readVisibleSectionsByActorCode("komenanyc@yahoo.fr").stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("102","321","327");
	}
	
	//@Test
	public void scopeQuerier_countVisibleSessionsByActorCode(){
		//assertThat(ScopeQuerier.getInstance().readVisibleSessionsByActorCode("1")).isNull();
		assertThat(ScopeQuerier.getInstance().countVisibleSectionsByActorCode("komenanyc@yahoo.fr")).isEqualTo(3l);
	}
	
	@Test
	public void scopeQuerier_readVisibleSectionsWhereFilter(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		Collection<Scope> scopes = ScopeQuerier.getInstance().readVisibleSectionsWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, "komenanyc@yahoo.fr")
				.addFilterField(ScopeQuerier.PARAMETER_NAME_NAME, "Constitutionnel Conseil"));
		assertThat(scopes).isNull();
		//assertThat(scopes.stream().map(Scope::getName).collect(Collectors.toList())).containsExactly("Conseil Constitutionnel");
	}
}