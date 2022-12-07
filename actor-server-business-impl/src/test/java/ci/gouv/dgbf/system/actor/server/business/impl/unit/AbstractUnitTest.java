package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTest extends org.cyk.utility.test.business.server.AbstractUnitTest {

	@Override
	protected void callInitialize() {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
		
	}
	
	/**/
	
	protected static Collection<Scope> readScopes(String typeCode,String actorCode,Boolean visible) {
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_TYPE_CODE, typeCode
						,ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode,ScopeQuerier.PARAMETER_NAME_VISIBLE, visible)
				.addProcessableTransientFieldsNames(Scope.FIELD_VISIBLE);
		return EntityReader.getInstance().readMany(Scope.class,queryExecutorArguments);
	}
	
	protected static void assertScopesCodes(Collection<Scope> scopes,String[] expectedCodes,Boolean exact) {
		if(scopes == null) {
			assertThat(expectedCodes).isNull();
		}else {			
			Collection<String> codes = scopes.stream().map(scope -> scope.getCode()).collect(Collectors.toList());
			assertThat(expectedCodes).as("expected scopes codes list is null. found "+codes).isNotNull();
			String message = "scopes codes are incorrects";
			if(Boolean.TRUE.equals(exact))
				assertThat(codes).as(message).containsExactly(expectedCodes);
			else
				assertThat(codes).as(message).contains(expectedCodes);
		}
	}
	
	protected static void assertScopesCodes(String typeCode,String actorCode,Boolean visible,String[] expectedCodes,Boolean exact) {
		assertScopesCodes(readScopes(typeCode, actorCode,visible),expectedCodes,exact);
	}
	
	protected static void assertScopesVisibles(Collection<Scope> scopes,Boolean[] expectedVisibles,Boolean exact) {
		if(scopes == null) {
			assertThat(expectedVisibles).isNull();
		}else {
			Collection<Boolean> visibles = scopes.stream().map(scope -> scope.getVisible()).collect(Collectors.toList());
			String message = "visibles are incorrects";
			if(Boolean.TRUE.equals(exact))
				assertThat(visibles).as(message).containsExactly(expectedVisibles);
			else
				assertThat(visibles).as(message).contains(expectedVisibles);
		}
	}
	
	protected static void assertScopesVisibles(String typeCode,String actorCode,Boolean visible,Boolean[] expectedVisibles,Boolean exact) {
		assertScopesVisibles(readScopes(typeCode, actorCode, visible),expectedVisibles,exact);
	}
	
	/**/
	
	protected static void assertScopes(String typeCode,Object[][] expectedCodesAndVisibes,String[] expectedVisiblesCodes,String[] expectedNotVisiblesCodes
			,Object[][] expectedCodesAndVisibesByActor,Object[][] expectedVisiblesCodesByActor,Object[][] expectedNotVisiblesCodesByActor
			,Boolean exact){	
		assertScopesCodesAndVisibes(typeCode, null,null,expectedCodesAndVisibes,exact);
		assertScopesCodes(typeCode, null, Boolean.TRUE,expectedVisiblesCodes,exact);
		assertScopesCodes(typeCode, null, Boolean.FALSE,expectedNotVisiblesCodes,exact);	
		
		if(expectedCodesAndVisibesByActor != null)
			for(Integer index = 0; index < expectedCodesAndVisibesByActor.length; index = index + 1)
				assertScopesCodesAndVisibes(typeCode, (String)expectedCodesAndVisibesByActor[index][0],null,(Object[][])expectedCodesAndVisibesByActor[index][1],exact);
		
		if(expectedVisiblesCodesByActor != null)
			for(Object[] array : expectedVisiblesCodesByActor)
				assertScopesCodes(typeCode,(String)array[0], Boolean.TRUE,(String[]) array[1],exact);
		if(expectedNotVisiblesCodesByActor != null)
			for(Object[] array : expectedNotVisiblesCodesByActor)
				assertScopesCodes(typeCode,(String)array[0], Boolean.FALSE,(String[]) array[1],exact);
	}
	
	protected static void assertScopesCodesAndVisibes(String typeCode,String actorCode,Boolean visible,Object[][] expectedCodesAndVisibes,Boolean exact) {
		Collection<Scope> scopes = readScopes(typeCode, actorCode, visible);
		String[] expectedCodes = new String[expectedCodesAndVisibes.length];
		Boolean[] expectedVisibles = new Boolean[expectedCodesAndVisibes.length];
		for(Integer index = 0; index < expectedCodesAndVisibes.length; index = index + 1) {
			expectedCodes[index] = (String) expectedCodesAndVisibes[index][0];
			expectedVisibles[index] = (Boolean) expectedCodesAndVisibes[index][1];
		}
		assertScopesCodes(scopes,expectedCodes,exact);
		assertScopesVisibles(scopes, expectedVisibles,exact);
	}

	/**/
	
	protected static void assertScopeFunction(String identifier,String expectedName,String expectedCategoryCode,String expectedBudgetCategoryIdentifier) {
		ScopeFunction scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, identifier);
		assertThat(scopeFunction).as(String.format("Scope function %s exists",identifier)).isNotNull();
		assertThat(scopeFunction.getCode()).isEqualTo(identifier);
		assertThat(scopeFunction.getName()).isEqualTo(expectedName);
		if(expectedCategoryCode == null) {
			assertThat(scopeFunction.getCategory()).as("category").isNull();
		}else {
			assertThat(scopeFunction.getCategory()).as("category").isNotNull();
			assertThat(scopeFunction.getCategory().getCode()).as("category code").isEqualTo(expectedCategoryCode);
		}
		
		if(expectedBudgetCategoryIdentifier == null) {
			assertThat(scopeFunction.getBudgetCategoryIdentifier()).as("budget category identifier").isNull();
		}else {
			assertThat(scopeFunction.getBudgetCategoryIdentifier()).as("budget category identifier").isEqualTo(expectedBudgetCategoryIdentifier);
		}
	}
	
	protected static void assertScopeFunction(String identifier,String expectedName) {
		assertScopeFunction(identifier, expectedName, null,null);
	}
	
	protected static void assertRequestDispatchSlipProcessingDateIsNull(String identifier,Boolean expectedValue) {
		RequestDispatchSlip requestDispatchSlip = EntityFinder.getInstance().find(RequestDispatchSlip.class, identifier);
		if(Boolean.TRUE.equals(expectedValue))
			assertThat(requestDispatchSlip.getProcessingDate()).as("processing date is null").isNull();
		else
			assertThat(requestDispatchSlip.getProcessingDate()).as("processing date is not null").isNotNull();
	}
}