package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;
import net.sf.ehcache.CacheManager;

public class PersistenceApiUnitTestDevPerformance extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void config_getMaxEntriesLocalHeap(){
		assertThat(CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(Section.class.getName()).getCacheConfiguration().getMaxEntriesLocalHeap()).isEqualTo(64);//specific
		assertThat(CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(AdministrativeUnit.class.getName()).getCacheConfiguration().getMaxEntriesLocalHeap()).isEqualTo(103);//default
	}
	
	@Test
	public void section_cache_query(){
		for(Integer index = 0 ; index < 100000 ; index = index + 1) {
			Integer initialSize = CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(Section.class.getName()).getSize();
			EntityManagerGetter.getInstance().get().createQuery("SELECT t FROM Section t WHERE t.identifier = 'SECTION01172e2c-7eb0-41a3-8d18-4c786e933ff7'", Section.class)
			.setHint("org.hibernate.cacheable", true)	
			.getSingleResult();
			Integer count = CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(Section.class.getName()).getSize() - initialSize;
			assertThat(count).isEqualTo(index == 0 ? 1 : 0);
		}
	}
	
	@Test
	public void section_cache_findByIdentifier(){
		for(Integer index = 0 ; index < 100 ; index = index + 1) {			
			Integer initialSize = CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(Section.class.getName()).getSize();
			EntityManagerGetter.getInstance().get().find(Section.class, "SECTION01172e2c-7eb0-41a3-8d18-4c786e933ff7");
			Integer count = CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(Section.class.getName()).getSize() - initialSize;
			assertThat(count).isEqualTo(index == 0 ? 1 : 0);
		}
	}
	
	@Test
	public void section_cache_expire_findByIdentifier(){
		assertThat(CacheManager.ALL_CACHE_MANAGERS.get(0).getCache(Section.class.getName()).getCacheConfiguration().getTimeToIdleSeconds()).isEqualTo(5);
		for(Integer t = 1; t <=10; t = t + 1) {
			System.out.println("Getting section");
			for(Integer index = 0 ; index < 10 ; index = index + 1)
				EntityManagerGetter.getInstance().get().find(Section.class, "SECTION01172e2c-7eb0-41a3-8d18-4c786e933ff7");
			System.out.println("Waiting "+t+" second(s)...");
			TimeHelper.pause(1000l * t);
		}
		
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
			AssignmentsQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments().setNumberOfTuples(count));
			Long d1 = System.currentTimeMillis() - t;
			
			System.out.println(count+" : "+TimeHelper.formatDuration(d1));
		}
	}
	
	@Test
	public void assignments_countWhereFilterUI(){
		for(Integer count : new Integer[] {1,2,3,4,5,10,20,25,50,100,250,500,1000,2500,5000,10000,20000,30000,50000,70000,80000,100000,120000}) {
			Long t = System.currentTimeMillis();
			AssignmentsQuerier.getInstance().countWhereFilter(new QueryExecutorArguments());
			Long d1 = System.currentTimeMillis() - t;
			
			System.out.println(count+" : "+TimeHelper.formatDuration(d1));
		}
	}
	
	@Test
	public void assignments_readWhereFilterUI_cache(){
		for(Integer count : new Integer[] {1,2,3,4,5,10,20,25,50,100,250,500,1000,2500,5000,10000,20000,30000,50000,70000,80000,100000,120000}) {
			Long t = System.currentTimeMillis();
			AssignmentsQuerier.getInstance().readWhereFilterForUI(new QueryExecutorArguments().setNumberOfTuples(count).setIsResultCachable(Boolean.TRUE));
			Long d1 = System.currentTimeMillis() - t;
			
			System.out.println(count+" : "+TimeHelper.formatDuration(d1));
		}
	}
	
	@Test
	public void executionImputation_readWhereFilter(){
		//org.cyk.utility.persistence.server.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		for(Integer count : new Integer[] {1,2,3,4,5,10,20,25,50,100,250,500,1000,2500,5000,10000,20000,30000,50000,70000,80000,100000,120000}) {
			Long t = System.currentTimeMillis();
			Collection<ExecutionImputation> executionImputations = EntityReader.getInstance().readMany(ExecutionImputation.class,new QueryExecutorArguments()
					.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)
					.setNumberOfTuples(count));
			System.out.println(String.format("%s read in %s", executionImputations.size(),TimeHelper.formatDuration(System.currentTimeMillis() - t)));
		}		
	}
}