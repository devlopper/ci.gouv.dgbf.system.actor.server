package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.test.business.server.Transaction;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.impl.AssignmentsBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.ScopeFunctionBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionCategory;

public class AssignmentsBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "assignments";
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_G() {
		assertThatCountIsEqualTo(ScopeFunction.class, 6l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("DBE","G1", null,"test",null, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(ScopeFunction.class, 8l);
		assertScopeFunction("G100001", "Gestionnaire de crédits DBE",ScopeFunctionCategory.CODE_G1,"BUD_GEN");
		assertScopeFunction("A1000010", "Assistant gestionnaire de crédits DBE",ScopeFunctionCategory.CODE_A1,"BUD_GEN");
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				entityManager.clear();
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("EPN",ScopeFunctionCategory.CODE_G6, null,"test",null, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(ScopeFunction.class, 10l);
		assertScopeFunction("G600002", "Gestionnaire de crédits EPN",ScopeFunctionCategory.CODE_G6,"BUD_EPN");
		assertScopeFunction("A1000020", "Assistant gestionnaire de crédits EPN",ScopeFunctionCategory.CODE_A1,"BUD_EPN");
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_O() {
		assertThatCountIsEqualTo(ScopeFunction.class, 6l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("22086","O2", null,"test",null, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(ScopeFunction.class, 8l);
		assertScopeFunction("O200001", "Ordonnateur délégué du Programme BUDGET");
		assertScopeFunction("A2000010", "Assistant ordonnateur délégué du Programme BUDGET");
		
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("22086","O3", "Ordonnateur délégué du Programme BUDGET à Dimbokro","test",null, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(ScopeFunction.class, 10l);
		assertScopeFunction("O300002", "Ordonnateur délégué du Programme BUDGET à Dimbokro");
		assertScopeFunction("A2000020", "Assistant Ordonnateur délégué du Programme BUDGET à Dimbokro");
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_C() {
		assertThatCountIsEqualTo(ScopeFunction.class, 6l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("DTI","C1", "C.F. de la section 01","test",null, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(ScopeFunction.class, 8l);
		assertScopeFunction("C100001", "C.F. de la section 01");
		assertScopeFunction("A3000010", "Assistant C.F. de la section 01");
	}
	
	@Test
	public void createByScopeIdentifierByCategoryCode_T() {
		assertThatCountIsEqualTo(ScopeFunction.class, 6l);
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("DTI","T1", "Trésorier du SUD Comoé","test",null, entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(ScopeFunction.class, 8l);
		assertScopeFunction("T100000", "Trésorier du SUD Comoé");
		assertScopeFunction("A4000000", "Assistant Trésorier du SUD Comoé");
	}
		
	//@Test
	public void createByScopeIdentifierByCategoryCode_sameMultiple() {
		Assertions.assertThrows(Exception.class, () -> {
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("DOCD","G1", null,"test",Boolean.TRUE, entityManager);
				}
			}.run();
			assertThatCountIsEqualTo(ScopeFunction.class, 8l);
			new Transaction.AbstractImpl() {
				@Override
				protected void __run__(EntityManager entityManager) {
					ScopeFunctionBusinessImpl.createByScopeIdentifierByCategoryCode("DOCD","G1", null,"test",Boolean.TRUE, entityManager);
				}
			}.run();
		});
	}
	
	@Test
	public void deriveValuesByIdentifiers_one() {
		assertThatCountIsEqualTo(Assignments.class, 3l);
		Assignments assignments = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(assignments.getCreditManagerHolder()).as("gc").isNull();
		assertThat(assignments.getCreditManagerAssistant()).as("agc").isNull();
		assertThat(assignments.getAuthorizingOfficerHolder()).as("ord").isNull();
		assertThat(assignments.getAuthorizingOfficerAssistant()).as("aord").isNull();
		assertThat(assignments.getFinancialControllerHolder()).as("cf").isNull();
		assertThat(assignments.getFinancialControllerAssistant()).as("acf").isNull();
		assertThat(assignments.getAccountingHolder()).as("cpt").isNull();
		assertThat(assignments.getAccountingAssistant()).as("acpt").isNull();
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.deriveValuesByIdentifiers(List.of("1"), Boolean.TRUE, Boolean.TRUE, null, "test", entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Assignments.class, 3l);
		assignments = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(assignments.getCreditManagerHolder()).as("gc").isNotNull();
		assertThat(assignments.getCreditManagerAssistant()).as("agc").isNotNull();
		assertThat(assignments.getAuthorizingOfficerHolder()).as("ord").isNotNull();
		assertThat(assignments.getAuthorizingOfficerAssistant()).as("aord").isNotNull();
		assertThat(assignments.getFinancialControllerHolder()).as("cf").isNotNull();
		assertThat(assignments.getFinancialControllerAssistant()).as("acf").isNotNull();
		//assertThat(assignments.getAccountingHolder()).as("cpt").isNotNull();
		//assertThat(assignments.getAccountingAssistant()).as("acpt").isNotNull();
	}
	
	@Test
	public void deriveValuesByIdentifiers_many() {
		assertThatCountIsEqualTo(Assignments.class, 3l);
		Assignments assignments01 = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(assignments01.getCreditManagerHolder()).as("gc").isNull();
		assertThat(assignments01.getCreditManagerAssistant()).as("agc").isNull();
		assertThat(assignments01.getAuthorizingOfficerHolder()).as("ord").isNull();
		assertThat(assignments01.getAuthorizingOfficerAssistant()).as("aord").isNull();
		assertThat(assignments01.getFinancialControllerHolder()).as("cf").isNull();
		assertThat(assignments01.getFinancialControllerAssistant()).as("acf").isNull();
		assertThat(assignments01.getAccountingHolder()).as("cpt").isNull();
		assertThat(assignments01.getAccountingAssistant()).as("acpt").isNull();
		
		Assignments assignments02 = EntityFinder.getInstance().find(Assignments.class, "2");
		assertThat(assignments02.getCreditManagerHolder()).as("gc").isNull();
		assertThat(assignments02.getCreditManagerAssistant()).as("agc").isNull();
		assertThat(assignments02.getAuthorizingOfficerHolder()).as("ord").isNull();
		assertThat(assignments02.getAuthorizingOfficerAssistant()).as("aord").isNull();
		assertThat(assignments02.getFinancialControllerHolder()).as("cf").isNull();
		assertThat(assignments02.getFinancialControllerAssistant()).as("acf").isNull();
		assertThat(assignments02.getAccountingHolder()).as("cpt").isNull();
		assertThat(assignments02.getAccountingAssistant()).as("acpt").isNull();
		new Transaction.AbstractImpl() {
			@Override
			protected void __run__(EntityManager entityManager) {
				AssignmentsBusinessImpl.deriveValuesByIdentifiers(List.of("1","2"), Boolean.TRUE, Boolean.TRUE, null, "test", entityManager);
			}
		}.run();
		assertThatCountIsEqualTo(Assignments.class, 3l);
		assignments01 = EntityFinder.getInstance().find(Assignments.class, "1");
		assertThat(assignments01.getCreditManagerHolder()).as("gc").isNotNull();
		assertThat(assignments01.getCreditManagerAssistant()).as("agc").isNotNull();
		assertThat(assignments01.getAuthorizingOfficerHolder()).as("ord").isNotNull();
		assertThat(assignments01.getAuthorizingOfficerAssistant()).as("aord").isNotNull();
		assertThat(assignments01.getFinancialControllerHolder()).as("cf").isNotNull();
		assertThat(assignments01.getFinancialControllerAssistant()).as("acf").isNotNull();
		//assertThat(assignments.getAccountingHolder()).as("cpt").isNotNull();
		//assertThat(assignments.getAccountingAssistant()).as("acpt").isNotNull();
		
		assignments02 = EntityFinder.getInstance().find(Assignments.class, "2");
		assertThat(assignments02.getCreditManagerHolder()).as("gc").isNotNull();
		assertThat(assignments02.getCreditManagerAssistant()).as("agc").isNotNull();
		assertThat(assignments02.getAuthorizingOfficerHolder()).as("ord").isNotNull();
		assertThat(assignments02.getAuthorizingOfficerAssistant()).as("aord").isNotNull();
		assertThat(assignments02.getFinancialControllerHolder()).as("cf").isNotNull();
		assertThat(assignments02.getFinancialControllerAssistant()).as("acf").isNotNull();
	}
}