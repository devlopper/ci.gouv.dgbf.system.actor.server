package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;

public class PersistenceApiUnitTestDevPerformance extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void assignments_readWhereFilter(){
		for(Integer count : new Integer[] {1,2,3,4,5,10,20,25,50,100,250,500,1000,2500,5000,10000,20000,30000,50000,70000,80000,100000,120000}) {
			Long t = System.currentTimeMillis();
			Collection<Assignments> collection = 
					AssignmentsQuerier.getInstance().readWhereFilter(new QueryExecutorArguments().setNumberOfTuples(count));
			System.out.println(CollectionHelper.getSize(collection)+" : "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
		}
	}
	
	@Test
	public void assignments_readWhereFilterUI(){
		for(Integer count : new Integer[] {1,2,3,4,5,10,20,25,50,100,250,500,1000,2500,5000,10000,20000,30000,50000,70000,80000,100000,120000}) {
			Long t = System.currentTimeMillis();
			AssignmentsQuerier.getInstance().readWhereFilter(new QueryExecutorArguments().setNumberOfTuples(count));
			Long d1 = System.currentTimeMillis() - t;
			t = System.currentTimeMillis();
			AssignmentsQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments().setNumberOfTuples(count));
			Long d2 = System.currentTimeMillis() - t;
			
			System.out.println(count+" : "+TimeHelper.formatDuration(d1)+" / "+TimeHelper.formatDuration(d2));
		}
	}
	
	@Test
	public void executionImputation_readWhereFilter(){
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		for(Integer count : new Integer[] {1,2,3,4,5,10,20,25,50,100,250,500,1000,2500,5000,10000,20000,30000,50000,70000,80000,100000,120000}) {
			Long t = System.currentTimeMillis();
			Collection<ExecutionImputation> executionImputations = EntityReader.getInstance().readMany(ExecutionImputation.class,new QueryExecutorArguments()
					.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)
					.setNumberOfTuples(count));
			System.out.println(String.format("%s read in %s", executionImputations.size(),TimeHelper.formatDuration(System.currentTimeMillis() - t)));
		}		
	}
}