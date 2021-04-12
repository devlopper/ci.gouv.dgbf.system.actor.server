package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class PersistenceUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeFunctionQuerier_readByIdentifierForUI(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI);
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_IDENTIFIER,"G100761");
		queryExecutorArguments.addProcessableTransientFieldsNames(ScopeFunction.FIELD_ACTORS_CODES);
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readOne(queryExecutorArguments);
		assertThat(scopeFunction.getIdentifier()).isNotBlank();
		assertThat(scopeFunction.getActorsCodes()).containsExactly("yay.diomande@budget.gouv.ci");
	}
}