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
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public abstract class AbstractUnitTestMemory extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
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
	
	protected static void assertVisibleScopesCodes(String actorCode,String scopeTypeCode,String code,String name,String[] expectedCodes) {
		Collection<Scope> scopes = EntityReader.getInstance().readMany(Scope.class,new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))
				.addFlags(ScopeQuerier.FLAG_VISIBLE)
				.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode,ScopeQuerier.PARAMETER_NAME_TYPE_CODE, scopeTypeCode
						,ScopeQuerier.PARAMETER_NAME_CODE,code,ScopeQuerier.PARAMETER_NAME_NAME,name));
		if(scopes == null) {
			assertThat(expectedCodes).isNull();
		}else {
			assertThat(scopes.stream().map(scope -> scope.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
		}
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
}