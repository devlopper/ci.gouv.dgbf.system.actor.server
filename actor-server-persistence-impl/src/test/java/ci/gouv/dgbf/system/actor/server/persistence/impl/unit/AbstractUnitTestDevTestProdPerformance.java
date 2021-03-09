package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryGetter;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;

public abstract class AbstractUnitTestDevTestProdPerformance extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterForUI(){
		for(Integer index = 0 ; index < 30 ; index = index + 1) {
			QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
			//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
			queryExecutorArguments.setNumberOfTuples(20);
			AssignmentsQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		}
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterUsingIdentifiersOnly(){
		Query query = QueryGetter.getInstance().get(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		System.out.println(query.getValue());
		for(Integer index = 0 ; index < 10 ; index = index + 1) {
			QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
			//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
			queryExecutorArguments.setNumberOfTuples(20);
			AssignmentsQuerier.getInstance().readWhereFilterUsingIdentifiersOnly(queryExecutorArguments);
		}
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterUsingIdentifiersOnly_dynamic(){
		for(Integer index = 0 ; index < 10 ; index = index + 1) {
			QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
			//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
			queryExecutorArguments.setNumberOfTuples(20);
			AssignmentsQuerier.getInstance().readWhereFilterUsingIdentifiersOnly(queryExecutorArguments);
		}
	}
}