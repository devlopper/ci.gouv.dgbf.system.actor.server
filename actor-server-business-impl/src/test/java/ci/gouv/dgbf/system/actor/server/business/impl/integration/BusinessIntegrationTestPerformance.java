package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.business.NativeQueryStringExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.server.persistence.test.arquillian.AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.business.api.ExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
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
	
	@Test
	public void executionImputation_readByIdentifierForEdit(){
		String identifier = "IMPUTATION11018010006BF1E3A5D576B48E1B5DC3D4FDA69A2CF";
		ExecutionImputation executionImputation = null;
		try {
			executionImputation = ExecutionImputationQuerier.getInstance().readByIdentifierForEdit(identifier);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		}
		assertThat(executionImputation.getCreditManager()).isNotNull();
		assertThat(executionImputation.getCreditManager().getHolderIdentifier()).isEqualTo("GC13010662");
		assertThat(executionImputation.getCreditManager().getAssistantIdentifier()).isEqualTo("AGC13010662");
		assertThat(executionImputation.getAuthorizingOfficer()).isNotNull();
		assertThat(executionImputation.getAuthorizingOfficer().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getAuthorizingOfficer().getAssistantIdentifier()).isNotBlank();
		assertThat(executionImputation.getFinancialController()).isNotNull();
		assertThat(executionImputation.getFinancialController().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getFinancialController().getAssistantIdentifier()).isNotBlank();
		assertThat(executionImputation.getAccounting()).isNotNull();
		assertThat(executionImputation.getAccounting().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getAccounting().getAssistantIdentifier()).isNotBlank();
		__inject__(ExecutionImputationBusiness.class).saveScopeFunctions(List.of(executionImputation));
		executionImputation = ExecutionImputationQuerier.getInstance().readByIdentifierForEdit(identifier);
		assertThat(executionImputation.getCreditManager()).isNotNull();
		assertThat(executionImputation.getCreditManager().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getCreditManager().getAssistantIdentifier()).isNotBlank();
		assertThat(executionImputation.getAuthorizingOfficer()).isNotNull();
		assertThat(executionImputation.getAuthorizingOfficer().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getAuthorizingOfficer().getAssistantIdentifier()).isNotBlank();
		assertThat(executionImputation.getFinancialController()).isNotNull();
		assertThat(executionImputation.getFinancialController().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getFinancialController().getAssistantIdentifier()).isNotBlank();
		assertThat(executionImputation.getAccounting()).isNotNull();
		assertThat(executionImputation.getAccounting().getHolderIdentifier()).isNotBlank();
		assertThat(executionImputation.getAccounting().getAssistantIdentifier()).isNotBlank();
	}
	
	//@Test
	public void all() throws Exception {
		try {
			Long t = System.currentTimeMillis();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deleteAll();
			
			__inject__(ScopeFunctionBusiness.class).deleteByFunctions(FunctionQuerier.getInstance().read());
			__inject__(ScopeFunctionBusiness.class).codifyAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).codifyAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).deleteByFunctions(FunctionQuerier.getInstance().read());
			
			__inject__(ScopeFunctionBusiness.class).deriveAll();
				
			ExecutionImputation executionImputation = new ExecutionImputation();
			executionImputation.getCreditManager(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			executionImputation.getAuthorizingOfficer(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			executionImputation.getFinancialController(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			executionImputation.getAccounting(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			Filter filter = new Filter();
			filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
			__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
			
			executionImputation = new ExecutionImputation();
			executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030040");
			filter = new Filter();
			filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
			__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
			
			executionImputation = new ExecutionImputation();
			executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030041").setHolderOverridable(Boolean.TRUE);
			filter = new Filter();
			filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
			__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
			
			Collection<ExecutionImputation> executionImputations = ExecutionImputationQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(10));
			executionImputations.forEach(index -> index.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030043"));
			__inject__(ExecutionImputationBusiness.class).saveScopeFunctions(executionImputations);
			
			executionImputations = ExecutionImputationQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(5));
			executionImputations.forEach(index -> index.getCreditManager(Boolean.TRUE).setHolderIdentifier(null));
			__inject__(ExecutionImputationBusiness.class).saveScopeFunctions(executionImputations);
			System.out.println("scopeFunction_all.DONE! "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void createPosteCF_initLign_CreatePosteORD_initLign() throws Exception {
		try {
			Long t = System.currentTimeMillis();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deleteAll();
			
			__inject__(ScopeFunctionBusiness.class).deriveByFunctions(FunctionQuerier.getInstance().read().stream()
					.filter(x -> x.getCode().equals(Function.CODE_FINANCIAL_CONTROLLER_HOLDER)).collect(Collectors.toList()));
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			
			__inject__(ScopeFunctionBusiness.class).deriveByFunctions(FunctionQuerier.getInstance().read().stream()
					.filter(x -> x.getCode().equals(Function.CODE_AUTHORIZING_OFFICER_HOLDER)).collect(Collectors.toList()));
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			
			System.out.println("createPosteCF_initLign_CreatePosteORD_initLign.DONE! "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void scopeFunction_all() throws Exception {
		try {
			Long t = System.currentTimeMillis();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
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
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
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
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deleteAll();
			__inject__(ScopeFunctionBusiness.class).deriveAll();
			__inject__(ScopeFunctionBusiness.class).codifyAll();
			__inject__(ScopeFunctionBusiness.class).codifyAll();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void executionImputation_deriveScopeFunctionsFromModel() throws Exception {
		try {			
			__inject__(NativeQueryStringExecutor.class).execute(new org.cyk.utility.__kernel__.persistence.query.NativeQueryStringExecutor.Arguments()
					.addQueriesStrings("DELETE FROM POSTE_IMPUTATION"));
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();			
			ExecutionImputation executionImputation = new ExecutionImputation();
			executionImputation.getCreditManager(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			executionImputation.getAuthorizingOfficer(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			executionImputation.getFinancialController(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			executionImputation.getAccounting(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
			Filter filter = new Filter();
			filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
			__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
			
			executionImputation = new ExecutionImputation();
			executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030040");
			filter = new Filter();
			filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
			__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
			
			executionImputation = new ExecutionImputation();
			executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030041").setHolderOverridable(Boolean.TRUE);
			filter = new Filter();
			filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
			__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
			
			Collection<ExecutionImputation> executionImputations = ExecutionImputationQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(10));
			executionImputations.forEach(index -> index.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030043"));
			__inject__(ExecutionImputationBusiness.class).saveScopeFunctions(executionImputations);
			
			executionImputations = ExecutionImputationQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).setNumberOfTuples(5));
			executionImputations.forEach(index -> index.getCreditManager(Boolean.TRUE).setHolderIdentifier(null));
			__inject__(ExecutionImputationBusiness.class).saveScopeFunctions(executionImputations);
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void scopeFunctionExecutionImputation_deriveAll() throws Exception {
		try {
			Long t = System.currentTimeMillis();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			System.out.println("scopeFunctionExecutionImputation_deriveAll.DONE! "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void scopeFunctionExecutionImputation_deriveAll_stress() throws Exception {
		try {
			Long t = System.currentTimeMillis();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
			__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
			System.out.println("scopeFunctionExecutionImputation_deriveAll_stress.DONE! "+TimeHelper.formatDuration(System.currentTimeMillis() - t));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void executionImputation_deriveScopeFunctionsFromModel_create_gc_holder() throws Exception {
		ExecutionImputation executionImputation = new ExecutionImputation();
		executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030040");
		Filter filter = new Filter();
		filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
		__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
	}
	
	//@Test
	public void executionImputation_deriveScopeFunctionsFromModel_update_gc_holder() throws Exception {
		ExecutionImputation executionImputation = new ExecutionImputation();
		executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier("GC11030041").setHolderOverridable(Boolean.TRUE);
		Filter filter = new Filter();
		filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
		__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
	}
	
	//@Test
	public void executionImputation_deriveScopeFunctionsFromModel_delete() throws Exception {
		__inject__(ScopeFunctionExecutionImputationBusiness.class).deleteAll();
		__inject__(ScopeFunctionBusiness.class).deleteAll();		
		__inject__(ScopeFunctionBusiness.class).deleteByFunctions(FunctionQuerier.getInstance().read());		
		__inject__(ScopeFunctionExecutionImputationBusiness.class).deriveAll();
		
		ExecutionImputation executionImputation = new ExecutionImputation();
		executionImputation.getCreditManager(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
		executionImputation.getAuthorizingOfficer(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
		executionImputation.getFinancialController(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
		executionImputation.getAccounting(Boolean.TRUE).setHolderOverridable(Boolean.TRUE);
		Filter filter = new Filter();
		filter.addField(ExecutionImputationQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE_NAME, "0");
		__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
	}
}