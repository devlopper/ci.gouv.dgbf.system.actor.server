package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.klass.PersistableClassesGetter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public abstract class AbstractPersistenceApiUnitTestValidate extends org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		//ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@SuppressWarnings("unchecked")
	//@Test
	public void run(){
		assertCountIsNotNull((Collection<Class<?>>) PersistableClassesGetter.COLLECTION.get());	
	}
	
	@Test
	public void scopeQuerier_readWhereFilterByTypesCodes_SECTION(){
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_TYPE_CODE, "SECTION")
				.addFilterField(ScopeQuerier.PARAMETER_NAME_NAME, "Constitutionnel Conseil"));
		assertThat(scopes).isNotNull();
		assertThat(scopes.stream().map(Scope::getName).collect(Collectors.toList())).containsExactly("Conseil Constitutionnel");
	}
	
	/**/
	
	public static void assertInvisibleSectionsWhereFilter(String actorCode,String code,String[] expectedCodes) {
		assertScopes(ScopeOfTypeSectionQuerier.getInstance().readInvisibleWhereFilter(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode,ScopeQuerier.PARAMETER_NAME_CODE, code)),expectedCodes);
	}
	
	public static void assertVisibleSectionsWhereFilter(String actorCode,String...expectedCodes) {
		assertScopes(EntityReader.getInstance().readMany(Scope.class,new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeOfTypeSectionQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER)
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	public static void assertInvisibleAdministrativeUnitsWhereFilter(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readInvisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	public static void assertVisibleAdministrativeUnitsWhereFilter(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeAdministrativeUnitQuerier.getInstance().readVisibleWithSectionsWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	public static void assertInvisibleBudgetSpecializationUnitsWhereFilter(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readInvisibleWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	public static void assertVisibleBudgetSpecializationUnitsWhereFilter(String actorCode,String...expectedCodes) {
		assertScopes(ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
				.addFilterField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode)),expectedCodes);
	}
	
	public static void assertScopes(Collection<Scope> scopes,String...expectedCodes) {
		if(CollectionHelper.isEmpty(scopes)) {
			assertThat(ArrayHelper.isEmpty(expectedCodes)).as("No scopes found").isTrue();
		}else {
			assertThat(scopes.stream().map(x -> x.getCode()).collect(Collectors.toList())).contains(expectedCodes);
		}
	}
}