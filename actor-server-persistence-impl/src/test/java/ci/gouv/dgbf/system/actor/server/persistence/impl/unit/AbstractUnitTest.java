package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.ArithmeticOperator;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Field;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {
	
	protected static Boolean EXCAT = Boolean.TRUE;
	
	@Override
	protected void callInitialize() {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
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
	
	/* Actors */
	
	protected static Collection<Actor> readActors(String code,String firstName,String lastNames,Boolean isActorScopeRequestWhereGrantedIsNullExist,String[] transientsFieldsNames,String[] flags) {
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		if(StringHelper.isNotBlank(code))
			queryExecutorArguments.getFilter(Boolean.TRUE).addFields(new Field().setName(ActorQuerier.PARAMETER_NAME_CODE).setValue(code).setArithmeticOperator(ArithmeticOperator.LIKE));
		if(StringHelper.isNotBlank(firstName))
			queryExecutorArguments.getFilter(Boolean.TRUE).addFields(new Field().setName(ActorQuerier.PARAMETER_NAME_FIRST_NAME).setValue(firstName).setArithmeticOperator(ArithmeticOperator.LIKE));
		if(StringHelper.isNotBlank(lastNames))
			queryExecutorArguments.getFilter(Boolean.TRUE).addFields(new Field().setName(ActorQuerier.PARAMETER_NAME_LAST_NAMES).setValue(lastNames).setArithmeticOperator(ArithmeticOperator.LIKE));
		if(isActorScopeRequestWhereGrantedIsNullExist != null)
			queryExecutorArguments.getFilter(Boolean.TRUE).addField(ActorQuerier.PARAMETER_NAME_IS_ACTOR_SCOPE_REQUEST_WHERE_GRANTED_IS_NULL_EXIST, isActorScopeRequestWhereGrantedIsNullExist);
		if(transientsFieldsNames != null)
			queryExecutorArguments.addProcessableTransientFieldsNames(transientsFieldsNames);
		if(flags != null)
			queryExecutorArguments.addFlags(flags);
		return EntityReader.getInstance().readMany(Actor.class,queryExecutorArguments);
	}
	
	protected static Collection<Actor> searchActors(String search,String[] transientsFieldsNames) {
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)).addFlags(ActorQuerier.FLAG_SEARCH);
		if(StringHelper.isNotBlank(search))
			queryExecutorArguments.addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_SEARCH,search);
		if(transientsFieldsNames != null)
			queryExecutorArguments.addProcessableTransientFieldsNames(transientsFieldsNames);
		return EntityReader.getInstance().readMany(Actor.class,queryExecutorArguments);
	}
	
	protected static void assertActors(Collection<Actor> actors,Object[][] expectedArrays,Boolean exact) {
		if(CollectionHelper.isEmpty(actors))
			assertThat(expectedArrays).isNull();
		else {
			for(Actor actor : actors) {
				Object[] expectedArray = null;
				for(Object[] array : expectedArrays) {
					if(array[0].equals(actor.getIdentifier())) {
						expectedArray = array;
						break;
					}
				}
				if(expectedArray == null && !Boolean.TRUE.equals(exact))
					continue;
				assertThat(actor).isNotNull();
				assertThat(expectedArray).as("expected array must not be null for actor "+actor.getIdentifier()).isNotNull();
				Integer index = 1;
				assertThat(actor.getCivilityAsString()).isEqualTo(expectedArray[index++]);
				assertThat(actor.getFirstName()).isEqualTo(expectedArray[index++]);
				assertThat(actor.getLastNames()).isEqualTo(expectedArray[index++]);	
				assertThat(actor.getGroupAsString()).isEqualTo(expectedArray[index++]);
				assertThat(actor.getElectronicMailAddress()).isEqualTo(expectedArray[index++]);
				assertThat(actor.getAdministrativeUnitAsString()).isEqualTo(expectedArray[index++]);
				assertThat(actor.getSectionAsString()).isEqualTo(expectedArray[index++]);
				assertThat(actor.getAdministrativeFunction()).isEqualTo(expectedArray[index++]);
				
				assertThat(actor.getNames()).isEqualTo(expectedArray[index++]);		
				
				//assertThat(actor.getProfilesCodes()).containsExactly((Collection<String>)expectedArray[index++]);
			}
		}
	}
	
	protected static void assertActors(String code,String firstName,String lastNames,Boolean isActorScopeRequestWhereGrantedIsNullExist,String[] transientsFieldsNames,String[] flags,Object[][] expectedArrays,Boolean exact) {
		assertActors(readActors(code, firstName, lastNames,isActorScopeRequestWhereGrantedIsNullExist, transientsFieldsNames,flags), expectedArrays, exact);
	}
	
	protected static void assertSearchActors(String search,String[] transientsFieldsNames,Object[][] expectedArrays,Boolean exact) {
		assertActors(searchActors(search, transientsFieldsNames), expectedArrays, exact);
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
				.addFilterField(ActorQuerier.PARAMETER_NAME_CODE,code).addProcessableTransientFieldsNames(Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_LAST_NAMES_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION
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
	
	protected static void assertScopesCodes(Collection<Scope> scopes,String[] expectedCodes) {
		assertScopesCodes(scopes, expectedCodes, EXCAT);
	}
	
	protected static void assertScopesCodes(String typeCode,String code,String name,Boolean visible,String actorCode,String[] expectedCodes,Boolean exact) {
		assertScopesCodes(readScopes(typeCode, code, name, visible, actorCode, null),expectedCodes,exact);
	}
	
	protected static void assertScopesCodes(String typeCode,String code,String name,Boolean visible,String actorCode,String[] expectedCodes) {
		assertScopesCodes(readScopes(typeCode, code, name, visible, actorCode, null),expectedCodes,EXCAT);
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
	
	protected static void assertScopesVisibles(Collection<Scope> scopes,Boolean[] expectedVisibles) {
		assertScopesVisibles(scopes, expectedVisibles, EXCAT);
	}
	
	protected static void assertScopesVisibles(String typeCode,String code,String name,Boolean visible,String actorCode,Boolean[] expectedVisibles,Boolean exact) {
		assertScopesVisibles(readScopes(typeCode, code, name, visible, actorCode, Boolean.TRUE),expectedVisibles,exact);
	}
	
	protected static void assertScopesVisibles(String typeCode,String code,String name,Boolean visible,String actorCode,Boolean[] expectedVisibles) {
		assertScopesVisibles(readScopes(typeCode, code, name, visible, actorCode, Boolean.TRUE),expectedVisibles,EXCAT);
	}
	
	/**/
	
	protected static void assertScopes(String typeCode,Object[][] expectedCodesAndVisibes,String[] expectedVisiblesCodes,String[] expectedNotVisiblesCodes
			,Object[] filteredCode,Object[] filteredName,Object[][] expectedCodesAndVisibesByActor,Object[][] expectedVisiblesCodesByActor
			,Object[][] expectedNotVisiblesCodesByActor,Object[][] expectedVisibleByActorsCodes,Boolean exact){	
		/*assertScopesCodesAndVisibes(typeCode, null,expectedCodesAndVisibes,exact);
		assertScopesCodes(typeCode, null, null, Boolean.TRUE,null,expectedVisiblesCodes,exact);
		assertScopesCodes(typeCode, null, null, Boolean.FALSE,null,expectedNotVisiblesCodes,exact);	
		if(filteredCode != null)
			assertScopesCodes(typeCode, (String)filteredCode[0], null, null,null,(String[]) filteredCode[1],exact);
		if(filteredName != null)
			assertScopesCodes(typeCode, null,(String)filteredName[0], null,null,(String[]) filteredName[1],exact);
		
		if(expectedCodesAndVisibesByActor != null) {
			for(Integer index = 0; index < expectedCodesAndVisibesByActor.length; index = index + 1)
				assertScopesCodesAndVisibes(typeCode, (String)expectedCodesAndVisibesByActor[index][0],(Object[][])expectedCodesAndVisibesByActor[index][1],exact);
		}
		
		if(expectedVisiblesCodesByActor != null)
			for(Object[] array : expectedVisiblesCodesByActor)
				assertScopesCodes(typeCode, null,null, Boolean.TRUE,(String)array[0],(String[]) array[1],exact);
		if(expectedNotVisiblesCodesByActor != null)
			for(Object[] array : expectedNotVisiblesCodesByActor)
				assertScopesCodes(typeCode, null,null, Boolean.FALSE,(String)array[0],(String[]) array[1],exact);
		*/
		if(expectedVisibleByActorsCodes != null)
			for(Object[] array : expectedVisibleByActorsCodes)
				assertScopeVisibleBy((String)array[0],typeCode,(String[])array[1]);
	}
	
	protected static void assertScopes(String typeCode,Object[][] expectedCodesAndVisibes,String[] expectedVisiblesCodes,String[] expectedNotVisiblesCodes
			,Object[] filteredCode,Object[] filteredName,Object[][] expectedCodesAndVisibesByActor,Object[][] expectedVisiblesCodesByActor
			,Object[][] expectedNotVisiblesCodesByActor,Object[][] expectedVisibleByActorsCodes){
		assertScopes(typeCode, expectedCodesAndVisibes, expectedVisiblesCodes, expectedNotVisiblesCodes, filteredCode, filteredName, expectedCodesAndVisibesByActor
				, expectedVisiblesCodesByActor, expectedNotVisiblesCodesByActor, expectedVisibleByActorsCodes,EXCAT);
	}
	
	protected static void assertScopesCodesAndVisibes(String typeCode,String actorCode,Object[][] expectedCodesAndVisibes,Boolean exact) {
		Collection<Scope> scopes = readScopes(typeCode, null, null, null, actorCode, Boolean.TRUE);
		String[] expectedCodes = new String[expectedCodesAndVisibes.length];
		Boolean[] expectedVisibles = new Boolean[expectedCodesAndVisibes.length];
		for(Integer index = 0; index < expectedCodesAndVisibes.length; index = index + 1) {
			expectedCodes[index] = (String) expectedCodesAndVisibes[index][0];
			expectedVisibles[index] = (Boolean) expectedCodesAndVisibes[index][1];
		}
		assertScopesCodes(scopes,expectedCodes,exact);
		assertScopesVisibles(scopes, expectedVisibles,exact);
	}

	protected static void assertScopeVisibleBy(String identifier,String typeCode,String...actorsCodes) {
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))
				.addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_IDENTIFIER, identifier,ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_TYPE_CODE, typeCode);
		Collection<Actor> actors = EntityReader.getInstance().readMany(Actor.class,queryExecutorArguments);
		if(actorsCodes == null)
			assertThat(actors).isNull();
		else {
			assertThat(actors).as(String.format("Il n'y pas d'acteur voyant %s %s", typeCode,identifier)).isNotNull();
			if(Boolean.TRUE.equals(EXCAT))
				assertThat(actors.stream().map(actor -> actor.getCode()).collect(Collectors.toList())).containsExactly(actorsCodes);
			else
				assertThat(actors.stream().map(actor -> actor.getCode()).collect(Collectors.toList())).contains(actorsCodes);
		}
	}
}