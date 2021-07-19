package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Override
	protected void callInitialize() {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		QueryHelper.clear();
		QueryManager.getInstance().clear();
	}
	
	protected static void assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(String string,String expectedCode) {
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readMaxCodeWhereCodeStartsWith(string);
		assertThat(scopeFunction).as("No scope function starting with "+string+" not found").isNotNull();
		assertThat(scopeFunction.getCode()).as(scopeFunction.getCode()+" is not equal to "+expectedCode).isEqualTo(expectedCode);
	}
	
	protected static void assert_scopeFunctionQuerier_readMaxOrderNumberByFunctionCode(String functionCode,Integer expectedOrderNumber) {
		Integer orderNumber = ScopeFunctionQuerier.getInstance().readMaxOrderNumberByFunctionCode(functionCode);
		assertThat(orderNumber).as("Max order number of "+functionCode).isEqualTo(expectedOrderNumber);
	}
	
	protected static void assertRequestScopeFunctionGranted(String identifier,Boolean expectedValue) {		
		RequestScopeFunction requestScopeFunction = EntityFinder.getInstance().find(RequestScopeFunction.class, identifier);
		assertThat(requestScopeFunction.getGranted()).as("granted").isEqualTo(expectedValue);
	}
	
	/**/
	
	protected static <T extends AuditableWhoDoneWhatWhen> void assertAudit(Class<T> klass,LocalDateTime fromDate,LocalDateTime toDate,QueryExecutorArguments queryExecutorArguments,Integer expectedCount) {
		//System.out.println("AbstractUnitTestMemory.assertAudit() "+fromDate+" - "+toDate+" : "+expectedCount);
		if(queryExecutorArguments == null) {
			queryExecutorArguments = new QueryExecutorArguments();
			queryExecutorArguments.addProjectionsFromStrings("identifier","__auditRevision__");
			queryExecutorArguments.setQuery(new Query().setIdentifier(QueryIdentifierBuilder.getInstance().build(klass, QueryName.READ_AUDIT)));
		}
		if(fromDate != null)
			queryExecutorArguments.addFilterField(Querier.PARAMETER_NAME_FROM_DATE, fromDate);
		if(toDate != null)
			queryExecutorArguments.addFilterField(Querier.PARAMETER_NAME_TO_DATE, toDate);
		
		Collection<T> audits = EntityReader.getInstance().readMany(klass, queryExecutorArguments);		
		assertThat(CollectionHelper.getSize(audits)).isEqualTo(expectedCount);
		if(CollectionHelper.isEmpty(audits))
			return;
		audits.forEach(audit -> {
			assertThat(FieldHelper.readSystemIdentifier(audit)).as("identifier is null").isNotNull();
			assertThat(audit.get__auditWhenAsString__()).as("when as string is null").isNotNull();
		});
	}
	
	protected static <T extends AuditableWhoDoneWhatWhen> void assertAudit(Class<T> klass,LocalDateTime fromDate,LocalDateTime toDate,Integer expectedCount) {
		assertAudit(klass, fromDate, toDate, null, expectedCount);
	}
	
	protected static void addPause(List<LocalDateTime> dateTimes,Integer duration) {
		if(dateTimes == null || NumberHelper.isLessThanOrEqualZero(duration))
			return;
		LocalDateTime date = null;
		//if(dateTimes.isEmpty())
			date = LocalDateTime.now();
		//else
		//	date = CollectionHelper.getLast(dateTimes).plus(duration.longValue(),ChronoField.MILLI_OF_DAY.getBaseUnit());
		dateTimes.add(date);
		TimeHelper.pause(duration.longValue());
	}
	
	protected static void addPause(List<LocalDateTime> dateTimes) {
		addPause(dateTimes, 100);
	}
	
	protected static void assertReadDynamicOne(QueryExecutorArguments queryExecutorArguments,String expectedCivility,String expectedFirstName,String expectedLastNames
			,String expectedGroup,String expectedElectronicMailAddress,String expectedAdministrativeUnitCodeName,String expectedSectionCodeName
			,String expectedAdministrativeUnitFunction,String[] profilesCodes) {
		Actor actor = EntityReader.getInstance().readOne(Actor.class, queryExecutorArguments);
		assertThat(actor).isNotNull();
		assertThat(actor.getCivilityAsString()).isEqualTo(expectedCivility);
		assertThat(actor.getFirstName()).isEqualTo(expectedFirstName);
		assertThat(actor.getLastNames()).isEqualTo(expectedLastNames);		
		assertThat(actor.getGroupAsString()).isEqualTo(expectedGroup);
		assertThat(actor.getElectronicMailAddress()).isEqualTo(expectedElectronicMailAddress);
		assertThat(actor.getAdministrativeUnitAsString()).isEqualTo(expectedAdministrativeUnitCodeName);
		assertThat(actor.getSectionAsString()).isEqualTo(expectedSectionCodeName);
		assertThat(actor.getAdministrativeFunction()).isEqualTo(expectedAdministrativeUnitFunction);
		
		assertThat(actor.getProfilesCodes()).containsExactly(profilesCodes);
	}
	
	protected static void assertReadDynamicOne(String code,String expectedCivility,String expectedFirstName,String expectedLastNames
			,String expectedGroup,String expectedElectronicMailAddress,String expectedAdministrativeUnitCodeName,String expectedSectionCodeName
			,String expectedAdministrativeUnitFunction,String[] expectedProfilesCodes) {
		assertReadDynamicOne(new QueryExecutorArguments().setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC_ONE))
				.addFilterField(ActorQuerier.PARAMETER_NAME_CODE,code).addProcessableTransientFieldsNames(Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION
						,Actor.FIELD_PROFILES_CODES), expectedCivility, expectedFirstName, expectedLastNames, expectedGroup, expectedElectronicMailAddress
				, expectedAdministrativeUnitCodeName, expectedSectionCodeName, expectedAdministrativeUnitFunction, expectedProfilesCodes);
	}
	
	/* Scope */
	
	protected static Collection<Scope> readScopes(String typeCode,String code,String name,Boolean visible,String actorCode,Boolean processFieldVisible) {
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))
				.addFilterFieldsValues(
						ScopeQuerier.PARAMETER_NAME_TYPE_CODE, typeCode
						,ScopeQuerier.PARAMETER_NAME_CODE,code
						,ScopeQuerier.PARAMETER_NAME_NAME,name
						,ScopeQuerier.PARAMETER_NAME_VISIBLE, visible
						,ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode						
					);
		if(Boolean.TRUE.equals(processFieldVisible))
			queryExecutorArguments.addProcessableTransientFieldsNames(Scope.FIELD_VISIBLE);
		return EntityReader.getInstance().readMany(Scope.class,queryExecutorArguments);
	}
	
	protected static void assertScopesCodes(Collection<Scope> scopes,String[] expectedCodes) {
		if(scopes == null) {
			assertThat(expectedCodes).isNull();
		}else {
			assertThat(scopes.stream().map(scope -> scope.getCode()).collect(Collectors.toList())).contains(expectedCodes);
			//assertThat(scopes.stream().map(scope -> scope.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
		}
	}
	
	protected static void assertScopesCodes(String typeCode,String code,String name,Boolean visible,String actorCode,String[] expectedCodes) {
		assertScopesCodes(readScopes(typeCode, code, name, visible, actorCode, null),expectedCodes);
	}
	
	protected static void assertScopesVisibles(Collection<Scope> scopes,Boolean[] expectedVisibles) {
		if(scopes == null) {
			assertThat(expectedVisibles).isNull();
		}else {
			assertThat(scopes.stream().map(scope -> scope.getVisible()).collect(Collectors.toList())).as("visibles are incorrects").contains(expectedVisibles);
			//assertThat(scopes.stream().map(scope -> scope.getVisible()).collect(Collectors.toList())).as("visibles are incorrects").containsExactly(expectedVisibles);
		}
	}
	
	protected static void assertScopesVisibles(String typeCode,String code,String name,Boolean visible,String actorCode,Boolean[] expectedVisibles) {
		assertScopesVisibles(readScopes(typeCode, code, name, visible, actorCode, Boolean.TRUE),expectedVisibles);
	}
	
	/**/
	
	protected static void assertVisibleScopesCodes(String actorCode,String scopeTypeCode,String code,String name,String[] expectedCodes) {
		assertScopesCodes(EntityReader.getInstance().readMany(Scope.class,new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode,ScopeQuerier.PARAMETER_NAME_TYPE_CODE, scopeTypeCode
						,ScopeQuerier.PARAMETER_NAME_CODE,code,ScopeQuerier.PARAMETER_NAME_NAME,name,ScopeQuerier.PARAMETER_NAME_VISIBLE,Boolean.TRUE)),expectedCodes);
	}
	
	protected static void assertVisibleSectionsCodes(String actorCode,String code,String name,String[] expectedCodes) {
		assertVisibleScopesCodes(actorCode, ScopeType.CODE_SECTION,code,name, expectedCodes);
	}
	
	protected static void assertVisibleAdministrativeUnitsCodes(String actorCode,String code,String name,String[] expectedCodes) {
		assertVisibleScopesCodes(actorCode, ScopeType.CODE_UA,code,name, expectedCodes);
	}
	
	protected static void assertVisibleBudgetSpecializationUnitsCodes(String actorCode,String code,String name,String[] expectedCodes) {
		assertVisibleScopesCodes(actorCode, ScopeType.CODE_USB,code,name, expectedCodes);
	}
	
	protected static void assertVisibleActionsCodes(String actorCode,String code,String name,String[] expectedCodes) {
		assertVisibleScopesCodes(actorCode, ScopeType.CODE_ACTION,code,name, expectedCodes);
	}
	
	protected static void assertVisibleActivitiesCodes(String actorCode,String code,String name,String[] expectedCodes) {
		assertVisibleScopesCodes(actorCode, ScopeType.CODE_ACTIVITE,code,name, expectedCodes);
	}
	
	/**/
	
	protected static void assertScopes(String typeCode,Object[][] expectedCodesAndVisibes,String[] expectedVisiblesCodes,String[] expectedNotVisiblesCodes
			,Object[] filteredCode,Object[] filteredName,Object[][] expectedCodesAndVisibesByActor,Object[][] expectedVisiblesCodesByActor,Object[][] expectedNotVisiblesCodesByActor){	
		assertScopesCodesAndVisibes(typeCode, null,expectedCodesAndVisibes);
		assertScopesCodes(typeCode, null, null, Boolean.TRUE,null,expectedVisiblesCodes);
		assertScopesCodes(typeCode, null, null, Boolean.FALSE,null,expectedNotVisiblesCodes);	
		if(filteredCode != null)
			assertScopesCodes(typeCode, (String)filteredCode[0], null, null,null,(String[]) filteredCode[1]);
		if(filteredCode != null)
			assertScopesCodes(typeCode, null,(String)filteredName[0], null,null,(String[]) filteredName[1]);
		
		if(expectedCodesAndVisibesByActor != null) {
			for(Integer index = 0; index < expectedCodesAndVisibesByActor.length; index = index + 1)
				assertScopesCodesAndVisibes(typeCode, (String)expectedCodesAndVisibesByActor[index][0],(Object[][])expectedCodesAndVisibesByActor[index][1]);
		}
		
		if(expectedVisiblesCodesByActor != null)
			for(Object[] array : expectedVisiblesCodesByActor)
				assertScopesCodes(typeCode, null,null, Boolean.TRUE,(String)array[0],(String[]) array[1]);
		if(expectedNotVisiblesCodesByActor != null)
			for(Object[] array : expectedNotVisiblesCodesByActor)
				assertScopesCodes(typeCode, null,null, Boolean.FALSE,(String)array[0],(String[]) array[1]);
	}
	
	protected static void assertScopesCodesAndVisibes(String typeCode,String actorCode,Object[][] expectedCodesAndVisibes) {
		Collection<Scope> scopes = readScopes(typeCode, null, null, null, actorCode, Boolean.TRUE);
		String[] expectedCodes = new String[expectedCodesAndVisibes.length];
		Boolean[] expectedVisibles = new Boolean[expectedCodesAndVisibes.length];
		for(Integer index = 0; index < expectedCodesAndVisibes.length; index = index + 1) {
			expectedCodes[index] = (String) expectedCodesAndVisibes[index][0];
			expectedVisibles[index] = (Boolean) expectedCodesAndVisibes[index][1];
		}
		assertScopesCodes(scopes,expectedCodes);
		assertScopesVisibles(scopes, expectedVisibles);
	}
}