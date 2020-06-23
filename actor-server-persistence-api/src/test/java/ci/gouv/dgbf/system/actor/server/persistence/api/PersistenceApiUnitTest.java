package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class PersistenceApiUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
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
		EntityCreator.getInstance().createManyInTransaction(new ScopeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(new Scope().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Scope().setCode("2").setName("2").setTypeFromIdentifier("1"),new Scope().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
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
				,new ActorScope().setActorFromIdentifier("3").setScopeFromIdentifier("2"));
	}
}