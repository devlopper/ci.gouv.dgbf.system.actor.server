package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import java.util.Collection;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.persistence.test.arquillian.AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public class BusinessIntegrationTestPerformance extends AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenPostConstruct__() {
		org.cyk.utility.__kernel__.klass.PersistableClassesGetter.COLLECTION.set(java.util.List.of(
				ProfileFunction.class,ProfilePrivilege.class
				,PrivilegeType.class
				,ActorProfile.class,ActorScope.class
				,ScopeFunction.class,ScopeTypeFunction.class
				,Function.class,FunctionType.class
				,Actor.class
				,Profile.class,ProfileType.class,ScopeType.class
				,AccountRequest.class
				,Identity.class
				,RejectedAccountRequest.class
				));
		super.__listenPostConstruct__();
	}
	
	@Override
	protected void __listenBefore__() {
		// TODO Auto-generated method stub
		//super.__listenBefore__();
	}
		
	//@Test
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
	
	//@Test
	public void scopeFunction_all() throws Exception {
		try {
			Long t = System.currentTimeMillis();
			__inject__(AssignmentsBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deleteAll();			
			__inject__(ScopeFunctionBusiness.class).deleteByFunctions(FunctionQuerier.getInstance().read());
			__inject__(ScopeFunctionBusiness.class).codifyAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).codifyAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).deleteByFunctions(FunctionQuerier.getInstance().read());
			System.out.println("scopeFunction_all.DONE! "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void scopeFunction_deriveAll() throws Exception {
		try {
			__inject__(AssignmentsBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deleteAll();	
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void scopeFunction_codifyAll() throws Exception {
		try {
			__inject__(AssignmentsBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).codifyAll();
			__inject__(ScopeFunctionBusiness.class).codifyAll();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}