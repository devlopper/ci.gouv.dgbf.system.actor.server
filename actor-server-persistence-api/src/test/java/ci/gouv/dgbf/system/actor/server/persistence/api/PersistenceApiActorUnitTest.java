package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentityQuerier;
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
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiActorUnitTest extends AbstractPersistenceUnitTest {
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
	public void readWhereFilter(){
		assertReadWhereFilter(null, null, null, null,null, "admin","u01","u02","u03");
		assertReadWhereFilter("admin", null, null, null,null, "admin");
		assertReadWhereFilter("u", null, null, null,null, "u01","u02","u03");
		assertReadWhereFilter("1", null, null, null,null, "u01");
		assertReadWhereFilter(null, "user", null, null,null, "u01","u02","u03");
		assertReadWhereFilter(null, null, null, "P1",null, "u01","u03");
		
		assertReadWhereFilter(null, null, null, null,"101", "u01","u03");
		assertReadWhereFilter(null, null, null, null,"327", "admin","u03");
		assertReadWhereFilter(null, null, null, null,"323", "u01","u02","u03");
	}
	
	@Test
	public void readProfileInformationsByCode(){
		Actor actor = ActorQuerier.getInstance().readProfileInformationsByCode("admin");
		assertThat(actor.getCivilityAsString()).isEqualTo("Monsieur");
		assertThat(actor.getFirstName()).isEqualTo("Komenan");
		assertThat(actor.getLastNames()).isEqualTo("Yao");		
		assertThat(actor.getGroupAsString()).isEqualTo("Fonctionnaire");
		assertThat(actor.getElectronicMailAddress()).isEqualTo("ky@m.com");
		assertThat(actor.getAdministrativeUnitAsString()).isEqualTo("1022 DTI");
		assertThat(actor.getSectionAsString()).isEqualTo("101 Representation Nationale");
		assertThat(actor.getAdministrativeFunction()).isEqualTo("Chef de service");
	}
	
	@Test
	public void readWithAllWhereFilter(){
		Actor actor = CollectionHelper.getFirst(ActorQuerier.getInstance().readWithAllWhereFilter(new QueryExecutorArguments()
				.setQueryFromIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_WITH_ALL_WHERE_FILTER)));
		assertThat(actor.getFirstName()).isEqualTo("Komenan");
		assertThat(actor.getLastNames()).isEqualTo("Yao");		
		assertThat(actor.getProfiles()).isNotEmpty();
		assertThat(actor.getVisibleModules()).isNotEmpty();
		assertThat(actor.getVisibleSections()).isNotEmpty();
	}
	
	/**/
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(
				new ScopeType().setCode(ScopeType.CODE_SECTION).setOrderNumber((byte)1)
				,new ScopeType().setCode(ScopeType.CODE_USB).setOrderNumber((byte)2)
				,new ScopeType().setCode(ScopeType.CODE_ACTIVITE).setOrderNumber((byte)3)
				,new ScopeType().setCode(ScopeType.CODE_IMPUTATION).setOrderNumber((byte)4)
				,new ScopeType().setCode(ScopeType.CODE_UA).setOrderNumber((byte)5)
				,new Civility().setCode("Monsieur"),new Civility().setCode("Madame"),new Civility().setCode("Docteur")
				,new IdentityGroup().setCode("Fonctionnaire"),new IdentityGroup().setCode("Contractuel")
				,new PrivilegeType().setCode(PrivilegeType.CODE_MODULE)
				,new FunctionType().setCode("A")
				,new ProfileType().setCode("A"));
		
		EntityCreator.getInstance().createManyInTransaction(
				new Function().setCode("F1").setTypeFromIdentifier("A")
				,new Function().setCode("F2").setTypeFromIdentifier("A")
				,new Function().setCode("F3").setTypeFromIdentifier("A")
				);
		
		EntityCreator.getInstance().createManyInTransaction(
				new Profile().setCode("P1").setTypeFromIdentifier("A")
				,new Profile().setCode("P2").setTypeFromIdentifier("A")
				,new Profile().setCode("P3").setTypeFromIdentifier("A")
				);
		
		//Sections
		createSection("101","Representation Nationale");
		createSection("327","Ministère du Budget");
		createSection("323","Ministère de l'intérieur");
		
		//UAs
		createAdministrativeUnit("1022","DTI", "101");
		
		//Modules
		createPrivileges("M1", null);
		createPrivileges("M2", null);
		createPrivileges("M3", null);
		
		//Actors
		createActor("admin","Monsieur","Komenan","Yao","Fonctionnaire","ky@m.com","Chef de service","1022");
		createActor("u01","Monsieur","user","01","Fonctionnaire","ky@m01.com","Chef de service","1022");
		createActor("u02","Monsieur","user","02","Fonctionnaire","ky@m02.com","Chef de service","1022");
		createActor("u03","Monsieur","user","03","Fonctionnaire","ky@m03.com","Chef de service","1022");
		
		createActorScopes("admin", "327");
		createProfilePrivileges("admin", "M1");
		createActorProfiles("admin", "P2");
		
		createActorScopes("u01", "101");
		createActorScopes("u01", "323");
		createActorProfiles("u01", "P1");
		
		createActorScopes("u02", "323");
		createActorProfiles("u02", "P2");
		createActorProfiles("u02", "P3");
		
		createActorScopes("u03", "101");
		createActorScopes("u03", "327");
		createActorScopes("u03", "323");
		createActorProfiles("u03", "P1");
		createActorProfiles("u03", "P3");
	}
	
	private void createSection(String code,String name) {
		Scope sectionScope = new Scope().setCode(code).setName(name).setTypeFromIdentifier(ScopeType.CODE_SECTION);
		EntityCreator.getInstance().createManyInTransaction(sectionScope);
		Section section = new Section().setCode(sectionScope.getIdentifier()).setName(name);
		EntityCreator.getInstance().createManyInTransaction(section);
	}
	
	private void createAdministrativeUnit(String code,String name,String sectionIdentifier) {
		Scope administrativeUnitScope = new Scope().setCode(code).setName(name).setTypeFromIdentifier(ScopeType.CODE_UA);
		AdministrativeUnit administrativeUnit = new AdministrativeUnit().setIdentifier(administrativeUnitScope.getIdentifier())
				.setCode(administrativeUnitScope.getCode()).setName(name).setSectionFromIdentifier(sectionIdentifier);
		EntityCreator.getInstance().createManyInTransaction(administrativeUnitScope,administrativeUnit);
	}
	
	private void createActor(String code,String civilityIdentifier,String firstName,String lastNames,String groupIdentifier,String electronicMailAddress,String administrativeFunction,String administrativeUnitIdentifier) {
		Identity identity = new Identity().setIdentifier(code).setFirstName(firstName).setLastNames(lastNames).setElectronicMailAddress(electronicMailAddress)
				.setAdministrativeFunction(administrativeFunction).setAdministrativeUnitFromIdentifier(administrativeUnitIdentifier)
				.setCivilityFromIdentifier(civilityIdentifier).setGroupFromIdentifier(groupIdentifier);
		EntityCreator.getInstance().createManyInTransaction(identity);
		Actor actor = new Actor().setCode(code).setIdentityFromIdentifier(identity.getIdentifier());
		EntityCreator.getInstance().createManyInTransaction(actor);
		Profile profile = new Profile().setCode(actor.getCode());
		EntityCreator.getInstance().createOneInTransaction(profile);
		EntityCreator.getInstance().createOneInTransaction(new ActorProfile().setActor(actor).setProfile(profile));
	}
	
	private void createPrivileges(String code,String parentIdentifier) {
		Privilege privilege = new Privilege().setCode(code).setTypeFromIdentifier(PrivilegeType.CODE_MODULE).setParentIdentifier(parentIdentifier);
		EntityCreator.getInstance().createManyInTransaction(privilege);
	}
	
	private void createProfilePrivileges(String profileIdentifier,String...privilegesIdentifiers) {
		Collection<Object> profilePrivileges = new ArrayList<>();
		for(String privilegeIdentifier : privilegesIdentifiers)
			profilePrivileges.add(new ProfilePrivilege().setProfileFromIdentifier(profileIdentifier).setPrivilegeFromIdentifier(privilegeIdentifier));
		EntityCreator.getInstance().createManyInTransaction(profilePrivileges);
	}
	
	/*private void createProfileFunctions(String profileIdentifier,String...functionIdentifiers) {
		Collection<Object> profileFunctions = new ArrayList<>();
		for(String functionIdentifier : functionIdentifiers)
			profileFunctions.add(new ProfileFunction().setProfileFromIdentifier(profileIdentifier).setFunctionFromIdentifier(functionIdentifier));
		EntityCreator.getInstance().createManyInTransaction(profileFunctions);
	}*/
	
	private void createActorScopes(String actorIdentifier,String...scopesIdentifiers) {
		Collection<Object> actorScopes = new ArrayList<>();
		for(String scopeIdentifier : scopesIdentifiers)
			actorScopes.add(new ActorScope().setActorFromIdentifier(actorIdentifier).setScopeFromIdentifier(scopeIdentifier));
		EntityCreator.getInstance().createManyInTransaction(actorScopes);
	}
	
	private void createActorProfiles(String actorIdentifier,String...profilesIdentifiers) {
		Collection<Object> actorProfiles = new ArrayList<>();
		for(String profileIdentifier : profilesIdentifiers)
			actorProfiles.add(new ActorProfile().setActorFromIdentifier(actorIdentifier).setProfileFromIdentifier(profileIdentifier));
		EntityCreator.getInstance().createManyInTransaction(actorProfiles);
	}
	
	/**/
	
	private void assertReadWhereFilter(String actorCode,String firstName,String lastNames,String profileCode,String visibleSectionCode ,String...expectedCodes) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().addFilterFieldsValues(
				IdentityQuerier.PARAMETER_NAME_CODE,actorCode,IdentityQuerier.PARAMETER_NAME_FIRST_NAME,firstName,IdentityQuerier.PARAMETER_NAME_LAST_NAMES,lastNames);
		if(StringHelper.isNotBlank(profileCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_PROFILE_CODE,profileCode);
		if(StringHelper.isNotBlank(visibleSectionCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_VISIBLE_SECTION_CODE,visibleSectionCode);
		Collection<Actor> actors = ActorQuerier.getInstance().readWhereFilter(arguments);
		if(ArrayHelper.isEmpty(expectedCodes)) {
			assertThat(actors).as("actors found").isNull();
		}else {
			assertThat(actors).as("actors not found for parameters AC="+actorCode+", FN="+firstName+", LN="+lastNames+", PC="+profileCode+", VSC="+visibleSectionCode).isNotNull()
;			assertThat(actors.stream().map(x -> x.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
		}
	}
}