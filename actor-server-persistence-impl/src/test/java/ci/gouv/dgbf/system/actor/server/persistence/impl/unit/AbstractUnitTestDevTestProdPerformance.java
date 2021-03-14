package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryGetter;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public abstract class AbstractUnitTestDevTestProdPerformance extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterForUI(){
		for(Integer index = 0 ; index < 3 ; index = index + 1) {
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
	
	@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterUsingIdentifiersOnly_dynamic(){
		//MetricsManager.getInstance().enable();
		for(Integer index = 0 ; index < 10 ; index = index + 1) {
			QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
			queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
			//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
			//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
			//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
			queryExecutorArguments.setNumberOfTuples(20);
			EntityReader.getInstance().readMany(Assignments.class, queryExecutorArguments);
			//Query Plan Cache Hit Count : 0
			//Query Plan Cache Miss Count : 0
			//System.out.println("Query Plan Cache Hit : "+MetricsManager.getInstance().get().get("Query Plan Cache Hit Count"));
			//System.out.println("Query Plan Cache Miss : "+MetricsManager.getInstance().get().get("Query Plan Cache Miss Count"));
		}
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_countWhereFilterUsingIdentifiersOnly_dynamic(){
		for(Integer index = 0 ; index < 3 ; index = index + 1) {
			QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
			queryExecutorArguments.setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
			queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
			//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
			queryExecutorArguments.setNumberOfTuples(20);
			EntityCounter.getInstance().count(Assignments.class, queryExecutorArguments);
		}
	}
}