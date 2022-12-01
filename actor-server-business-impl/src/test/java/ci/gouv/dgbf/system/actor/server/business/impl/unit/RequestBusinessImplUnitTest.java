package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.RequestBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class RequestBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "requests";
	}
	
	@Test
	public void initialize() {
		String code = RandomHelper.getAlphanumeric(5);
		assertThatCountIsEqualTo(Request.class, 1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				Request request = new Request().setCode(code);
	        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
	        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")));
	        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
				RequestBusinessImpl.initialize(request, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Request.class, 2l);
	}
	
	@Test
	public void initialize_scopeFunctions_comptatible() {
		String code = RandomHelper.getAlphanumeric(5);
		assertThatCountIsEqualTo(Request.class, 1l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				Request request = new Request().setCode(code);
	        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
	        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")
	        			,EntityFinder.getInstance().find(ScopeFunction.class, "GDBE")
	        			));
	        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
				RequestBusinessImpl.initialize(request, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Request.class, 2l);
	}
	
	@Test
	public void initialize_scopeFunctions_incomptatible_throwException() {
		String code = RandomHelper.getAlphanumeric(5);
		assertThatCountIsEqualTo(Request.class, 1l);
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					Request request = new Request().setCode(code);
		        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
		        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")
		        			,EntityFinder.getInstance().find(ScopeFunction.class, "GCHUYOP")
		        			));
		        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
					RequestBusinessImpl.initialize(request, entityManager);
				}
			}.run();
		});
		assertThatCountIsEqualTo(Request.class, 1l);
	}
	
	@Test
    public void accept_scopeFunctions_incomptatible_throwException() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					RequestBusinessImpl.acceptByIdentifier("2", List.of("GDSIB","GCHUYOP"), null, null, "meliane", entityManager);
				}
			}.run();
		}); 	
    }
}