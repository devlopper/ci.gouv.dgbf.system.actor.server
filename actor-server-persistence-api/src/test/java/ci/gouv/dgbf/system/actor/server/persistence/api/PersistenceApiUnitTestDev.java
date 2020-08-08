package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.logging.Level;

import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.junit.jupiter.api.Test;

public class PersistenceApiUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
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