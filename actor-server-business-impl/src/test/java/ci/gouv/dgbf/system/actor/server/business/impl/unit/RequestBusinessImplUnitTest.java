package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.RequestBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.RequestDispatchSlipBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

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
	public void request_initialize() {
		String code = RandomHelper.getAlphanumeric(5);
		Long count = __inject__(EntityManagerGetter.class).get().createQuery("SELECT COUNT(t) FROM Request t",Long.class).getSingleResult();
		assertThatCountIsEqualTo(Request.class, count);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				Request request = new Request().setCode(code).setBudgetCategoryIdentifier("1");
	        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
	        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")));
	        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
				RequestBusinessImpl.initialize(request, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Request.class, count+1);
	}
	
	@Test
	public void request_initialize_scopeFunctions_comptatible() {
		String code = RandomHelper.getAlphanumeric(5);
		Long count = __inject__(EntityManagerGetter.class).get().createQuery("SELECT COUNT(t) FROM Request t",Long.class).getSingleResult();
		assertThatCountIsEqualTo(Request.class, count);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				Request request = new Request().setCode(code).setBudgetCategoryIdentifier("1");
	        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
	        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")
	        			,EntityFinder.getInstance().find(ScopeFunction.class, "GDBE")
	        			));
	        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
				RequestBusinessImpl.initialize(request, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Request.class, count+1);
	}
	
	@Test
	public void request_initialize_scopeFunctions_incomptatible_throwException() {
		String code = RandomHelper.getAlphanumeric(5);
		Long count = __inject__(EntityManagerGetter.class).get().createQuery("SELECT COUNT(t) FROM Request t",Long.class).getSingleResult();
		assertThatCountIsEqualTo(Request.class, count);
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					Request request = new Request().setCode(code).setBudgetCategoryIdentifier("1");
		        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
		        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")
		        			,EntityFinder.getInstance().find(ScopeFunction.class, "GCHUYOP")
		        			));
		        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
					RequestBusinessImpl.initialize(request, entityManager);
				}
			}.run();
		});
		assertThatCountIsEqualTo(Request.class, count);
	}
	
	@Test
    public void request_accept_scopeFunctions_incomptatible_throwException() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					RequestBusinessImpl.acceptByIdentifier("2", List.of("GDSIB","GCHUYOP"), null, null, "meliane", entityManager);
				}
			}.run();
		}); 	
    }
	
	@Test
	public void request_havingDispatchSlipSent_processAllRequest_processingDateShouldBeNotNull() {
		assertRequestDispatchSlipProcessingDateIsNull("1", Boolean.TRUE);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				RequestBusinessImpl.acceptByIdentifier("1", List.of("GDTI"), null, null, null, entityManager);
			}
		}.run();
		assertRequestDispatchSlipProcessingDateIsNull("1", Boolean.FALSE);
	}
	
	/**/
	
	@Test
	public void requestDispatchSlip_record() {
		String requestCode = RandomHelper.getAlphanumeric(5);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				Request request = new Request().setCode(requestCode).setBudgetCategoryIdentifier("1");
	        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m@mail.com");
	        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "GDSIB")));
	        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "DSIB"));
				RequestBusinessImpl.initialize(request, entityManager);
			}
		}.run();
		
		Long count = __inject__(EntityManagerGetter.class).get().createQuery("SELECT COUNT(t) FROM RequestDispatchSlip t",Long.class).getSingleResult();
		assertThatCountIsEqualTo(RequestDispatchSlip.class, count);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				RequestDispatchSlip requestDispatchSlip = new RequestDispatchSlip();
				requestDispatchSlip.setBudgetCategoryIdentifier("1");
				requestDispatchSlip.setSection(EntityFinder.getInstance().find(Section.class, "335"));
				requestDispatchSlip.setFunction(EntityFinder.getInstance().find(Function.class, "GC"));
				requestDispatchSlip.setRequests(List.of(CodeExecutor.getInstance().getOne(Request.class, requestCode)));
	        	RequestDispatchSlipBusinessImpl.record(requestDispatchSlip, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(RequestDispatchSlip.class, count+1);
	}
}