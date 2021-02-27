package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

public class PersistenceApiProfileUnitTest extends AbstractPersistenceUnitTest {
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
	public void profileQuerier_readWhereTypeIsSysteme(){
		assertThat(ProfileQuerier.getInstance().readWhereTypeIsSystemeOrderByCodeAscending().stream().map(Profile::getCode)
				.collect(Collectors.toList())).containsExactly("Sys01");
		assertThat(ProfileQuerier.getInstance().countWhereTypeIsSystemeOrderByCodeAscending()).isEqualTo(1l);
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
		assertThat(PrivilegeQuerier.getInstance().readByActorsCodes(List.of("4")).stream().map(Privilege::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3");
		assertThat(PrivilegeQuerier.getInstance().countByActorsCodes(List.of("4"))).isEqualTo(3l);
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
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createManyInTransaction(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createManyInTransaction(new ProfileType().setCode("1").setName("1"),new ProfileType().setCode("2").setName("2")
				,new ProfileType().setCode(ProfileType.CODE_SYSTEME));
		EntityCreator.getInstance().createManyInTransaction(
				new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1")
				,new Profile().setCode("3").setName("3").setTypeFromIdentifier("1")
				,new Profile().setCode("4").setName("3").setTypeFromIdentifier("1")
				,new Profile().setCode("5").setName("3").setTypeFromIdentifier("1")
				,new Profile().setCode("Sys01").setTypeFromIdentifier(ProfileType.CODE_SYSTEME));
		
		EntityCreator.getInstance().createManyInTransaction(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2"),new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("2"));
		
		EntityCreator.getInstance().createManyInTransaction(
				new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("3")
				,new ProfilePrivilege().setProfileFromIdentifier("2").setPrivilegeFromIdentifier("2")
				,new ProfilePrivilege().setProfileFromIdentifier("4").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("5").setPrivilegeFromIdentifier("1"));
		
		EntityCreator.getInstance().createManyInTransaction(
				new Actor().setCode("1").setFirstName("konan").setLastNames("marc")
				,new Actor().setCode("2").setFirstName("yao").setLastNames("jules")
				,new Actor().setCode("3").setFirstName("yao").setLastNames("jules")
				,new Actor().setCode("4").setFirstName("yaya").setLastNames("jules"));
		
		EntityCreator.getInstance().createManyInTransaction(
				new ActorProfile().setActorFromIdentifier("1").setProfileFromIdentifier("1")
				,new ActorProfile().setActorFromIdentifier("2").setProfileFromIdentifier("2")
				,new ActorProfile().setActorFromIdentifier("3").setProfileFromIdentifier("3")
				,new ActorProfile().setActorFromIdentifier("4").setProfileFromIdentifier("1")
				,new ActorProfile().setActorFromIdentifier("4").setProfileFromIdentifier("2")
				,new ActorProfile().setActorFromIdentifier("4").setProfileFromIdentifier("4")
				,new ActorProfile().setActorFromIdentifier("4").setProfileFromIdentifier("5"));
	}
}