package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTestValidate extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	//@Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	//@Test
	public void request_readTransients_accountCreationDate_accountCreationMessage(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setProcessableTransientFieldsNames(List.of(Request.FIELD_ACCOUNT_CREATION_DATE_AS_STRING));
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		requests.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getAccountCreationDateAsString()+" | "+x.getAccountCreationMessage());
		});
	}
	
	@Test
	public void scopeFunction_readTransients_actorsNames(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setProcessableTransientFieldsNames(List.of(ScopeFunction.FIELD_ACTORS_AS_STRINGS));
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getActorsCodes()+" | "+x.getActorsAsStrings());
		});
	}
}