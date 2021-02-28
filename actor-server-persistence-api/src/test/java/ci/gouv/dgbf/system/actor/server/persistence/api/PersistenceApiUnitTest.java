package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequestFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void accountRequestQuerier_readProjection01ByAccessToken(){
		LocalDate d1 = LocalDate.of(2020, 02, 03);
		LocalDateTime d2 = LocalDateTime.of(2021, 01, 05,00,00);
		LocalDateTime d3 = LocalDateTime.of(2021, 01, 05,14,35);
		EntityCreator.getInstance().createManyInTransaction(new Section().setIdentifier("SEC01"));
		EntityCreator.getInstance().createManyInTransaction(new Scope().setCode("11010045").setName("DTI"));
		EntityCreator.getInstance().createManyInTransaction(new AdministrativeUnit().setIdentifier("11010045").setSectionFromIdentifier("SEC01"));
		EntityCreator.getInstance().createManyInTransaction(new IdentityGroup().setCode("1").setName("Fonctionnaire"),new Civility().setCode("1").setName("Monsieur"));
		EntityCreator.getInstance().createOneInTransaction(new Identity().setIdentifier("1").setActOfAppointmentReference("Ref05").setActOfAppointmentSignatory("KG")
				.setActOfAppointmentSignatureDate(d1).setAdministrativeFunction("CE").setAdministrativeUnitFromIdentifier("11010045")
				.setCivilityFromIdentifier("1").setElectronicMailAddress("m").setFirstName("kom").setGroupFromIdentifier("1").setLastNames("yao")
				.setMobilePhoneNumber("01020304").setOfficePhoneExtension("01").setOfficePhoneNumber("22").setPostalBoxAddress("06BP")
				.setRegistrationNumber("100A"));
		EntityCreator.getInstance().createOneInTransaction(new AccountRequest().setIdentifier("1").setIdentityFromIdentifier("1").setCreationDate(d2).setAccessToken("at")
				.setSubmissionDate(d3));
		AccountRequest accountRequest = AccountRequestQuerier.getInstance().readProjection01ByAccessToken("at");	
		assertThat(accountRequest).isNotNull();
		assertThat(accountRequest.getAccessToken()).isEqualTo("at");
		assertThat(accountRequest.getActOfAppointmentReference()).isEqualTo("Ref05");
		assertThat(accountRequest.getActOfAppointmentSignatory()).isEqualTo("KG");
		assertThat(accountRequest.getActOfAppointmentSignatureDateAsString()).isEqualTo("03/02/2020");
		assertThat(accountRequest.getAdministrativeFunction()).isEqualTo("CE");
		assertThat(accountRequest.getAdministrativeUnitAsString()).isEqualTo("11010045 DTI");
		assertThat(accountRequest.getCivilityAsString()).isEqualTo("Monsieur");
		assertThat(accountRequest.getCreationDateAsString()).isEqualTo("05/01/2021 à 00:00");
		assertThat(accountRequest.getElectronicMailAddress()).isEqualTo("m");
		assertThat(accountRequest.getFirstName()).isEqualTo("kom");
		assertThat(accountRequest.getGroupAsString()).isEqualTo("Fonctionnaire");
		assertThat(accountRequest.getLastNames()).isEqualTo("yao");
		assertThat(accountRequest.getMobilePhoneNumber()).isEqualTo("01020304");
		assertThat(accountRequest.getOfficePhoneExtension()).isEqualTo("01");
		assertThat(accountRequest.getOfficePhoneNumber()).isEqualTo("22");
		assertThat(accountRequest.getPostalBoxAddress()).isEqualTo("06BP");
		assertThat(accountRequest.getRegistrationNumber()).isEqualTo("100A");
		assertThat(accountRequest.getSubmissionDateAsString()).isEqualTo("05/01/2021 à 14:35");
		assertThat(accountRequest.getFunctions()).isNull();
	}
	
	@Test
	public void accountRequestQuerier_readProjection01WithBudgetaryFunctionsAndFunctionsByAccessToken(){
		LocalDate d1 = LocalDate.of(2020, 02, 03);
		LocalDateTime d2 = LocalDateTime.of(2021, 01, 05,00,00);
		LocalDateTime d3 = LocalDateTime.of(2021, 01, 05,14,35);
		String functionTypeIdentifier = RandomHelper.getAlphabetic(3);
		String functionIdentifier = RandomHelper.getAlphabetic(3);
		EntityCreator.getInstance().createManyInTransaction(new FunctionType().setCode(functionTypeIdentifier).setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Function().setCode(functionIdentifier).setName("1").setTypeFromIdentifier(functionTypeIdentifier));
		EntityCreator.getInstance().createManyInTransaction(new Section().setIdentifier("SEC01"));
		EntityCreator.getInstance().createManyInTransaction(new Scope().setCode("11010045").setName("DTI"));
		EntityCreator.getInstance().createManyInTransaction(new AdministrativeUnit().setIdentifier("11010045").setSectionFromIdentifier("SEC01"));
		EntityCreator.getInstance().createManyInTransaction(new IdentityGroup().setCode("1").setName("Fonctionnaire"),new Civility().setCode("1").setName("Monsieur"));
		EntityCreator.getInstance().createOneInTransaction(new Identity().setIdentifier("1").setActOfAppointmentReference("Ref05").setActOfAppointmentSignatory("KG")
				.setActOfAppointmentSignatureDate(d1).setAdministrativeFunction("CE").setAdministrativeUnitFromIdentifier("11010045")
				.setCivilityFromIdentifier("1").setElectronicMailAddress("m").setFirstName("kom").setGroupFromIdentifier("1").setLastNames("yao")
				.setMobilePhoneNumber("01020304").setOfficePhoneExtension("01").setOfficePhoneNumber("22").setPostalBoxAddress("06BP")
				.setRegistrationNumber("100A"));
		EntityCreator.getInstance().createOneInTransaction(new AccountRequest().setIdentifier("1").setIdentityFromIdentifier("1").setCreationDate(d2).setAccessToken("at")
				.setSubmissionDate(d3));
		EntityCreator.getInstance().createOneInTransaction(new AccountRequestFunction().setAccountRequestFromIdentifier("1").setFunctionFromIdentifier(functionIdentifier));
		AccountRequest accountRequest = AccountRequestQuerier.getInstance().readProjection01WithBudgetaryFunctionsAndFunctionsByAccessToken("at");	
		assertThat(accountRequest).isNotNull();
		assertThat(accountRequest.getAccessToken()).isEqualTo("at");
		assertThat(accountRequest.getActOfAppointmentReference()).isEqualTo("Ref05");
		assertThat(accountRequest.getActOfAppointmentSignatory()).isEqualTo("KG");
		assertThat(accountRequest.getActOfAppointmentSignatureDateAsString()).isEqualTo("03/02/2020");
		assertThat(accountRequest.getAdministrativeFunction()).isEqualTo("CE");
		assertThat(accountRequest.getAdministrativeUnitAsString()).isEqualTo("11010045 DTI");
		assertThat(accountRequest.getCivilityAsString()).isEqualTo("Monsieur");
		assertThat(accountRequest.getCreationDateAsString()).isEqualTo("05/01/2021 à 00:00");
		assertThat(accountRequest.getElectronicMailAddress()).isEqualTo("m");
		assertThat(accountRequest.getFirstName()).isEqualTo("kom");
		assertThat(accountRequest.getGroupAsString()).isEqualTo("Fonctionnaire");
		assertThat(accountRequest.getLastNames()).isEqualTo("yao");
		assertThat(accountRequest.getMobilePhoneNumber()).isEqualTo("01020304");
		assertThat(accountRequest.getOfficePhoneExtension()).isEqualTo("01");
		assertThat(accountRequest.getOfficePhoneNumber()).isEqualTo("22");
		assertThat(accountRequest.getPostalBoxAddress()).isEqualTo("06BP");
		assertThat(accountRequest.getRegistrationNumber()).isEqualTo("100A");
		assertThat(accountRequest.getSubmissionDateAsString()).isEqualTo("05/01/2021 à 14:35");
		assertThat(accountRequest.getFunctions()).isNotNull();
	}
	
	@Test
	public void accountRequestQuerier_readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier(){
		LocalDate d1 = LocalDate.of(2020, 02, 03);
		LocalDateTime d2 = LocalDateTime.of(2021, 01, 05,00,00);
		LocalDateTime d3 = LocalDateTime.of(2021, 01, 05,14,35);
		String functionTypeIdentifier = RandomHelper.getAlphabetic(3);
		String functionIdentifier = RandomHelper.getAlphabetic(3);
		EntityCreator.getInstance().createManyInTransaction(new FunctionType().setCode(functionTypeIdentifier).setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Function().setCode(functionIdentifier).setName("1").setTypeFromIdentifier(functionTypeIdentifier));
		EntityCreator.getInstance().createManyInTransaction(new Section().setIdentifier("SEC01"));
		EntityCreator.getInstance().createManyInTransaction(new Scope().setCode("11010045").setName("DTI"));
		EntityCreator.getInstance().createManyInTransaction(new AdministrativeUnit().setIdentifier("11010045").setSectionFromIdentifier("SEC01"));
		EntityCreator.getInstance().createManyInTransaction(new IdentityGroup().setCode("1").setName("Fonctionnaire"),new Civility().setCode("1").setName("Monsieur"));
		EntityCreator.getInstance().createOneInTransaction(new Identity().setIdentifier("1").setActOfAppointmentReference("Ref05").setActOfAppointmentSignatory("KG")
				.setActOfAppointmentSignatureDate(d1).setAdministrativeFunction("CE").setAdministrativeUnitFromIdentifier("11010045")
				.setCivilityFromIdentifier("1").setElectronicMailAddress("m").setFirstName("kom").setGroupFromIdentifier("1").setLastNames("yao")
				.setMobilePhoneNumber("01020304").setOfficePhoneExtension("01").setOfficePhoneNumber("22").setPostalBoxAddress("06BP")
				.setRegistrationNumber("100A"));
		EntityCreator.getInstance().createOneInTransaction(new AccountRequest().setIdentifier("1").setIdentityFromIdentifier("1").setCreationDate(d2).setAccessToken("at")
				.setSubmissionDate(d3));
		EntityCreator.getInstance().createOneInTransaction(new AccountRequestFunction().setAccountRequestFromIdentifier("1").setFunctionFromIdentifier(functionIdentifier));
		AccountRequest accountRequest = AccountRequestQuerier.getInstance().readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier("1");	
		assertThat(accountRequest).isNotNull();
		assertThat(accountRequest.getAccessToken()).isEqualTo("at");
		assertThat(accountRequest.getActOfAppointmentReference()).isEqualTo("Ref05");
		assertThat(accountRequest.getActOfAppointmentSignatory()).isEqualTo("KG");
		assertThat(accountRequest.getActOfAppointmentSignatureDateAsString()).isNull();
		assertThat(accountRequest.getActOfAppointmentSignatureDateAsTimestamp()).isEqualTo(d1.atTime(0, 0).toInstant(ZoneOffset.UTC).toEpochMilli());
		assertThat(accountRequest.getAdministrativeFunction()).isEqualTo("CE");
		assertThat(accountRequest.getAdministrativeUnitAsString()).isNull();
		assertThat(accountRequest.getAdministrativeUnit().getIdentifier()).isEqualTo("11010045");
		assertThat(accountRequest.getAdministrativeUnit().getName()).isEqualTo("DTI");
		assertThat(accountRequest.getCivilityAsString()).isNull();
		assertThat(accountRequest.getCivility()).isNotNull();
		assertThat(accountRequest.getCivility().getName()).isEqualTo("Monsieur");
		assertThat(accountRequest.getCreationDateAsString()).isEqualTo("05/01/2021 à 00:00");
		assertThat(accountRequest.getElectronicMailAddress()).isEqualTo("m");
		assertThat(accountRequest.getFirstName()).isEqualTo("kom");
		assertThat(accountRequest.getGroupAsString()).isNull();
		assertThat(accountRequest.getGroup().getName()).isEqualTo("Fonctionnaire");
		assertThat(accountRequest.getLastNames()).isEqualTo("yao");
		assertThat(accountRequest.getMobilePhoneNumber()).isEqualTo("01020304");
		assertThat(accountRequest.getOfficePhoneExtension()).isEqualTo("01");
		assertThat(accountRequest.getOfficePhoneNumber()).isEqualTo("22");
		assertThat(accountRequest.getPostalBoxAddress()).isEqualTo("06BP");
		assertThat(accountRequest.getRegistrationNumber()).isEqualTo("100A");
		assertThat(accountRequest.getSubmissionDateAsString()).isEqualTo("05/01/2021 à 14:35");
		assertThat(accountRequest.getFunctions()).isNotNull();
	}
	
	//@Test
	public void actorQuerier_readAll01(){
		Collection<Actor> actors = EntityReader.getInstance().readMany(Actor.class, ActorQuerier.QUERY_IDENTIFIER_READ_ALL_01);
		assertThat(actors.iterator().next().getNames()).isEqualTo("konan marc");
	}
	
	@Test
	public void profileQuerier_readByTypesCodesByFunctionsCodes(){
		assertThat(ProfileQuerier.getInstance().readByTypesCodesByFunctionsCodes(List.of("1"), List.of("1")).stream().map(Profile::getCode)
				.collect(Collectors.toList())).containsExactly("1");
		assertThat(ProfileQuerier.getInstance().readByTypesCodesByFunctionsCodes(List.of("1"), List.of("2")).stream().map(Profile::getCode)
				.collect(Collectors.toList())).containsExactly("2","3");
	}
	
	@Test
	public void profileQuerier_readByActorsCodes(){
		assertThat(ProfileQuerier.getInstance().readByActorsCodes(List.of("1")).stream().map(Profile::getCode)
				.collect(Collectors.toList())).containsExactly("1");
		assertThat(ProfileQuerier.getInstance().readByActorsCodes(List.of("2")).stream().map(Profile::getCode)
				.collect(Collectors.toList())).containsExactly("2");
		assertThat(ProfileQuerier.getInstance().readByActorsCodes(List.of("3")).stream().map(Profile::getCode)
				.collect(Collectors.toList())).containsExactly("3");
	}
	
	@Test
	public void privilegeQuerier_readByProfilesTypesCodesByFunctionsCodes(){
		assertThat(PrivilegeQuerier.getInstance().readByProfilesTypesCodesByFunctionsCodes(List.of("1"), List.of("1")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("1","3");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesTypesCodesByFunctionsCodes(List.of("1"), List.of("2")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("2");
	}
	
	@Test
	public void privilegeQuerier_readByActorsCodes(){
		assertThat(PrivilegeQuerier.getInstance().readByActorsCodes(List.of("1")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("1","3");
		assertThat(PrivilegeQuerier.getInstance().readByActorsCodes(List.of("2")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByActorsCodes(List.of("3"))).isNull();
	}
	
	@Test
	public void privilegeQuerier_readByProfilesCodesNotAssociated(){
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodesNotAssociated(List.of("1")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodesNotAssociated(List.of("2")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("1","3");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodesNotAssociated(List.of("3")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3");
	}
	
	//@Test
	public void scopeQuerier_readWhereFilterByTypesCodes(){
		assertThat(ScopeQuerier.QUERY_VALUE_READ_WHERE_FILTER)
			.isEqualTo("SELECT t.identifier,t.code,t.name FROM Scope t WHERE"
					// type (code ,name)
					+ " LOWER(t.type.code) LIKE LOWER(:typeCode)"
					+ " AND (LOWER(t.type.name) LIKE LOWER(:typeName) OR (LOWER(t.type.name) LIKE LOWER(:typeName0) AND LOWER(t.type.name) LIKE LOWER(:typeName1) AND LOWER(t.type.name) LIKE LOWER(:typeName2)))"
					// (code,name)
					+ " AND LOWER(t.code) LIKE LOWER(:code)"
					+ " AND (LOWER(t.name) LIKE LOWER(:name) OR (LOWER(t.name) LIKE LOWER(:name0) AND LOWER(t.name) LIKE LOWER(:name1) AND LOWER(t.name) LIKE LOWER(:name2)))"
					// ordering
					+ " ORDER BY t.type.code ASC,t.code ASC");
		
		QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		Collection<Scope> scopes = ScopeQuerier.getInstance().readWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ScopeQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER).addFilterField(ScopeQuerier.PARAMETER_NAME_THIS, "a"));
		System.out.println(scopes);
	}
	
	@Test
	public void scopeQuerier_readByActorsCodesByTypesCodes(){
		assertThat(ScopeQuerier.getInstance().readByActorsCodesByTypesCodes(List.of("1"),List.of("1")).stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("1","3");
		assertThat(ScopeQuerier.getInstance().readByActorsCodesByTypesCodes(List.of("2"),List.of("1")).stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3");
		assertThat(ScopeQuerier.getInstance().readByActorsCodesByTypesCodes(List.of("3"),List.of("1")).stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("2");
	}
	
	@Test
	public void scopeQuerier_countByActorsCodesByTypesCodes(){
		assertThat(ScopeQuerier.getInstance().countByActorsCodesByTypesCodes(List.of("1"),List.of("1"))).isEqualTo(2l);
		assertThat(ScopeQuerier.getInstance().countByActorsCodesByTypesCodes(List.of("2"),List.of("1"))).isEqualTo(3l);
		assertThat(ScopeQuerier.getInstance().countByActorsCodesByTypesCodes(List.of("3"),List.of("1"))).isEqualTo(1l);
	}
	
	@Test
	public void scopeQuerier_readByActorsCodesNotAssociatedByTypesCodes(){
		assertThat(ScopeQuerier.getInstance().readByActorsCodesNotAssociatedByTypesCodes(List.of("1"),List.of("1")).stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("2");
		assertThat(ScopeQuerier.getInstance().readByActorsCodesNotAssociatedByTypesCodes(List.of("2"),List.of("1"))).isNull();
		assertThat(ScopeQuerier.getInstance().readByActorsCodesNotAssociatedByTypesCodes(List.of("3"),List.of("1")).stream().map(Scope::getCode)
				.collect(Collectors.toList())).containsExactly("1","3");
	}
	
	@Test
	public void scopeQuerier_countByActorsCodesNotAssociatedByTypesCodes(){
		assertThat(ScopeQuerier.getInstance().countByActorsCodesNotAssociatedByTypesCodes(List.of("1"),List.of("1"))).isEqualTo(1l);
		assertThat(ScopeQuerier.getInstance().countByActorsCodesNotAssociatedByTypesCodes(List.of("2"),List.of("1"))).isEqualTo(0l);
		assertThat(ScopeQuerier.getInstance().countByActorsCodesNotAssociatedByTypesCodes(List.of("3"),List.of("1"))).isEqualTo(2l);
	}
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new ScopeType().setCode("1").setName("1"),new ScopeType().setCode(ScopeType.CODE_SECTION).setName("Session")
				,new ScopeType().setCode(ScopeType.CODE_UA).setName("ua"));
		EntityCreator.getInstance().createManyInTransaction(new Scope().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Scope().setCode("2").setName("2").setTypeFromIdentifier("1"),new Scope().setCode("3").setName("3").setTypeFromIdentifier("1")
				,new Scope().setCode("4").setName("4").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("5").setName("4").setTypeFromIdentifier(ScopeType.CODE_SECTION)
				,new Scope().setCode("ua01").setName("5").setTypeFromIdentifier(ScopeType.CODE_UA));
		
		EntityCreator.getInstance().createManyInTransaction(new Section().setCode("1"),new Section().setCode("2"),new Section().setCode("3")
				,new Section().setCode("4"),new Section().setCode("5"));
		EntityCreator.getInstance().createManyInTransaction(new AdministrativeUnit().setCode("ua01").setSectionFromIdentifier("5")
				,new AdministrativeUnit().setCode("ua02").setSectionFromIdentifier("3"));
		
		EntityCreator.getInstance().createManyInTransaction(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createManyInTransaction(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createManyInTransaction(new ProfileType().setCode("1").setName("1"),new ProfileType().setCode("2").setName("2"));
		EntityCreator.getInstance().createManyInTransaction(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createManyInTransaction(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2"),new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("2"));
		
		EntityCreator.getInstance().createManyInTransaction(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("3"),new ProfilePrivilege().setProfileFromIdentifier("2").setPrivilegeFromIdentifier("2"));
		
		EntityCreator.getInstance().createManyInTransaction(new Actor().setCode("1").setFirstName("konan").setLastNames("marc")
				,new Actor().setCode("2").setFirstName("yao").setLastNames("jules"),new Actor().setCode("3").setFirstName("yao").setLastNames("jules"));
		
		EntityCreator.getInstance().createManyInTransaction(new ActorProfile().setActorFromIdentifier("1").setProfileFromIdentifier("1")
				,new ActorProfile().setActorFromIdentifier("2").setProfileFromIdentifier("2"),new ActorProfile().setActorFromIdentifier("3").setProfileFromIdentifier("3"));
		
		EntityCreator.getInstance().createManyInTransaction(
				new ActorScope().setActorFromIdentifier("1").setScopeFromIdentifier("1")
				,new ActorScope().setActorFromIdentifier("1").setScopeFromIdentifier("3")
				,new ActorScope().setActorFromIdentifier("2").setScopeFromIdentifier("1")
				,new ActorScope().setActorFromIdentifier("2").setScopeFromIdentifier("2")
				,new ActorScope().setActorFromIdentifier("2").setScopeFromIdentifier("3")
				,new ActorScope().setActorFromIdentifier("3").setScopeFromIdentifier("2")
				,new ActorScope().setActorFromIdentifier("3").setScopeFromIdentifier("4")
				,new ActorScope().setActorFromIdentifier("3").setScopeFromIdentifier("ua01")
				);
	}
}