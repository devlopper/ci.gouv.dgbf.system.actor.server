package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.EntityManagerGetter;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExecutionImputationQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public class PersistenceApiUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	//@Test
	public void createScopeFunctionExecutionImputation_natively(){
		EntityManager entityManager = EntityManagerGetter.getInstance().get();
		Collection<Object> objects = new ArrayList<Object>();
		objects.add(new ScopeFunctionExecutionImputation()
				.setScopeFunction(EntityFinder.getInstance().find(ScopeFunction.class, "GC13010662"))
				.setExecutionImputation(EntityFinder.getInstance().find(ExecutionImputation.class, "IMPUTATION11018010005BF1E3A5D576B48E1B5DC3D4FDA69A2CF")));
		entityManager.getTransaction().begin();
		EntityCreator.getInstance().createMany(new QueryExecutorArguments().setEntityManager(entityManager).setObjects(objects).setIsNative(Boolean.TRUE));
		entityManager.getTransaction().commit();
		//(new Identity().setFirstName("a").setLastNames("b").setElectronicMailAddress("a@b.com"));
	}
	
	@Test
	public void readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		System.out.println(AccountRequestQuerier.getInstance().readProjection01WithBudgetaryFunctionsAndFunctionsByIdentifier("D_yy@y.com"));
	}
	
	@Test
	public void scopeFunction_readByFunctionCodes(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		ScopeFunctionQuerier.getInstance().readByFunctionsCodes(Function.CODE_FINANCIAL_CONTROLLER_HOLDER).forEach(x -> {System.out.println(x);});
	}
	
	@Test
	public void scopeTypeFunction_read(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		System.out.println(EntityReader.getInstance().readMany(ScopeTypeFunction.class,ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ));
		System.out.println(EntityReader.getInstance().readMany(ScopeTypeFunction.class,ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_READ_FOR_UI));
		System.out.println(EntityCounter.getInstance().count(ScopeTypeFunction.class,ScopeTypeFunctionQuerier.QUERY_IDENTIFIER_COUNT));
		System.out.println(ScopeTypeFunctionQuerier.getInstance().readWhereScopeFunctionDerivableIsTrue());
	}
	
	@Test
	public void function_readByBusinessIdentifiers(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		FunctionQuerier.getInstance().readByBusinessIdentifiers(Function.class,List.of(Function.CODE_CREDIT_MANAGER_HOLDER
				,Function.CODE_AUTHORIZING_OFFICER_HOLDER,Function.CODE_FINANCIAL_CONTROLLER_HOLDER,Function.CODE_ACCOUNTING_HOLDER))
			.forEach(x -> {System.out.println(x);});
	}
	
	@Test
	public void executionImputation_readWhereFilter(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		Long t = System.currentTimeMillis();
		System.out.println("STARTS");
		Collection<ExecutionImputation> executionImputations = EntityReader.getInstance().readMany(ExecutionImputation.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)
				.setNumberOfTuples(20));
		executionImputations.forEach(x -> {
					System.out.println(x.getIdentifier()+" - "+ x.getCreditManagerHolderCodeName()+" : "+x.getAuthorizingOfficerHolderCodeName()
							+" : "+x.getFinancialControllerHolderCodeName()+" : "+x.getAccountingHolderCodeName());
				});
		System.out.println((System.currentTimeMillis() - t)/1000);
	}
	
	@Test
	public void executionImputation_readWhereFilterAll(){
		org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
		Long t = System.currentTimeMillis();
		System.out.println("STARTS");
		Collection<ExecutionImputation> executionImputations = EntityReader.getInstance().readMany(ExecutionImputation.class,new QueryExecutorArguments().setQuery(new Query().setIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_WITH_ALL))
				.setNumberOfTuples(20));
		executionImputations.forEach(x -> {
					System.out.println(x.getIdentifier()+" - "+ x.getCreditManager().getHolder().getCode()+" : "+x.getAuthorizingOfficer().getHolder().getCode()
							+" : "+x.getFinancialController().getHolder().getCode()+" : "+x.getAccounting().getHolder().getCode());
				});
		System.out.println((System.currentTimeMillis() - t)/1000);
	}
	
	@Test
	public void executionImputation_readAll(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		ExecutionImputationQuerier.getInstance().read();
		//__inject__(ExecutionImputationPersistence.class).read();
	}
	
	@Test
	public void executionImputation_readForEdit(){
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		ExecutionImputation executionImputation = EntityReader.getInstance().readOne(ExecutionImputation.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ExecutionImputationQuerier.QUERY_IDENTIFIER_READ_BY_SYSTEM_IDENTIFIER_FOR_EDIT)
				.addFilterFieldsValues(ExecutionImputationQuerier.PARAMETER_NAME_IDENTIFIER,"IMPUTATION11018010005BF1E3A5D576B48E1B5DC3D4FDA69A2CF"));
		System.out.println(executionImputation.getName());
		System.out.println(executionImputation.getFunctions());
	}
	
	@Test
	public void showFunctionWithAll(){
		System.out.println("--------------------- Function with all ---------------------");
		Collection<Function> functions = EntityReader.getInstance().readMany(Function.class, new QueryExecutorArguments().setQuery(new Query()
				.setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_READ_WHERE_ASSOCIATED_TO_SCOPE_TYPE_WITH_ALL)));
		if(CollectionHelper.isNotEmpty(functions))
			functions.forEach(function -> {
				System.out.println(function+" : "+function.getScopeTypes());
			});
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereCodeOrNameLikeAndNotAssociatedToFunctionByTypeIdentifier(
				new QueryExecutorArguments().setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_CODE_OR_NAME_LIKE_AND_NOT_ASSOCIATED_TO_FUNCTION_BY_TYPE_IDENTIFIER)
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,"CF",ScopeQuerier.PARAMETER_NAME_TYPE_IDENTIFIER,"SECTION"));
		if(CollectionHelper.isNotEmpty(scopes)) {
			System.out.println(scopes.size());
			//scopes.forEach(scope -> {
			//	System.out.println(scope);
			//});
		}
	}
	
	@Test
	public void counts(){
		System.out.println("--------------------- Counts ---------------------");
		System.out.println("Actors : "+EntityCounter.getInstance().count(Actor.class, ActorQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER));
		System.out.println("AccountRequests : "+EntityCounter.getInstance().count(AccountRequest.class, AccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER));
		System.out.println("RejectedAccountRequests : "+EntityCounter.getInstance().count(RejectedAccountRequest.class, RejectedAccountRequestQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER));
	}
	
	@Test
	public void scopeQuerier_USBs(){
		System.out.println("--------------------- USBs ---------------------");
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereTypeIsUSBAndFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_USB_AND_FILTER));
		if(scopes != null)
			scopes.forEach(scope -> {
				System.out.println(scope.getCode()+" : "+scope.getSectionAsString());
			});
	}
	
	@Test
	public void scopeQuerier_ACTIVITYs(){
		System.out.println("--------------------- ACTIVITYs ---------------------");
		Collection<Scope> scopes = ScopeOfTypeActivityQuerier.getInstance().readWhereFilter(new QueryExecutorArguments());
		if(scopes != null)
			System.out.println("COUNT : "+scopes.size());
			/*scopes.forEach(scope -> {
				System.out.println(scope.getCode()+" : "+scope.getSectionAsString());
			});*/
	}
	
	@Test
	public void scopeQuerier_readVisibleSectionsWhereFilter(){
		assertVisibleSectionsWhereFilter("kycdev@gmail.com","327","103");
		assertVisibleSectionsWhereFilter("kb@m.com","108");
	}
	
	@Test
	public void scopeQuerier_readInvisibleSectionsWhereFilter(){
		//assertInvisibleSectionsWhereFilter("kycdev@gmail.com",null,new String[] {"101","102"});
		//assertInvisibleSectionsWhereFilter("kb@m.com",null,new String[] {"102","103","103"});
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		assertInvisibleSectionsWhereFilter("kb@m.com","102",new String[] {});
	}
	
	@Test
	public void scopeQuerier_readVisibleAdministrativeUnitsWhereFilter(){
		assertVisibleAdministrativeUnitsWhereFilter("kycdev@gmail.com","11010016");
	}
	
	@Test
	public void scopeQuerier_readInvisibleAdministrativeUnitsWhereFilter(){
		assertInvisibleAdministrativeUnitsWhereFilter("kycdev@gmail.com","11010001");
	}
	
	@Test
	public void scopeQuerier_readVisibleBudgetSpecializationUnitsWhereFilter(){
		assertVisibleBudgetSpecializationUnitsWhereFilter("kycdev@gmail.com","14970","14984","15022","21083","22084","13004");
	}
	
	@Test
	public void scopeQuerier_readInvisibleBudgetSpecializationUnitsWhereFilter(){
		assertInvisibleBudgetSpecializationUnitsWhereFilter("kycdev@gmail.com","01001");
	}
}