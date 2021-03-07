package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.computation.SortOrder;
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
	public void requestQuerier_section101(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER
				,"SECTION01172e2c-7eb0-41a3-8d18-4c786e933ff7");
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		requests.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getSectionAsString());
		});
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
	
	//@Test
	public void scopeFunction_readTransients_actorsNames(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setProcessableTransientFieldsNames(List.of(ScopeFunction.FIELD_ACTORS_AS_STRINGS));
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getActorsCodes()+" | "+x.getActorsAsStrings());
		});
	}
	
	//@Test
	public void requestQuerier_readWhereFilter_sortByFirstName_asc(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setSortOrders(Map.of(RequestQuerier.PARAMETER_NAME_FIRST_NAME,SortOrder.DESCENDING));
		queryExecutorArguments.setNumberOfTuples(5);
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilter(queryExecutorArguments);
		requests.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getFirstName()+" | "+x.getLastNames());
		});
	}
}