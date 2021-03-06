package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;
import java.util.logging.Level;

import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RejectedAccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public class PersistenceApiUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	//@Test
	public void create(){
		EntityCreator.getInstance().createOneInTransaction(new Identity().setFirstName("a").setLastNames("b").setElectronicMailAddress("a@b.com"));
	}
	
	@Test
	public void readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier(){
		QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		System.out.println(AccountRequestQuerier.getInstance().readProjection01WithBudgetaryFunctionsAndFunctionsByIdentifier("D_yy@y.com"));
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
		QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
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