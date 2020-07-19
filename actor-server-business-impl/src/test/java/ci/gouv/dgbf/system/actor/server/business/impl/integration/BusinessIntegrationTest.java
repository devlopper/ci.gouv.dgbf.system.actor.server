package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.business.EntityCreator;
import org.cyk.utility.server.business.test.arquillian.AbstractBusinessArquillianIntegrationTestWithDefaultDeployment;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.RejectedAccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentityPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.RejectedAccountRequestPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;

public class BusinessIntegrationTest extends AbstractBusinessArquillianIntegrationTestWithDefaultDeployment {
	private static final long serialVersionUID = 1L;
	
	/* Create */
	
	@Test
	public void actor_create() throws Exception{
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode(ProfileType.CODE_SYSTEME).setName("1")
				,new ProfileType().setCode(ProfileType.CODE_UTILISATEUR).setName("2"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier(ProfileType.CODE_SYSTEME)
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier(ProfileType.CODE_SYSTEME)
				,new Profile().setCode("3").setName("3").setTypeFromIdentifier(ProfileType.CODE_SYSTEME));
		
		EntityCreator.getInstance().createMany(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2"),new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("2"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("3"),new ProfilePrivilege().setProfileFromIdentifier("2").setPrivilegeFromIdentifier("2"));
		
		Long profileCount = __inject__(ProfilePersistence.class).count();
		Long actorProfileCount = __inject__(ActorProfilePersistence.class).count();
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		__inject__(ActorBusiness.class).create(new Actor().setFirstName("yao").setLastNames("yves").setElectronicMailAddress("a@m.com")
				.addFunctionsByIdentifiers("1").setKeycloakUserCreatable(Boolean.FALSE));
		assertThat(__inject__(ProfilePersistence.class).count()).isEqualTo(profileCount+1);
		assertThat(__inject__(ActorProfilePersistence.class).count()).isEqualTo(actorProfileCount+1);
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+2);
	}
	
	@Test
	public void accountRequest_validate() throws Exception{
		EntityCreator.getInstance().createMany(new ProfileType().setCode(ProfileType.CODE_UTILISATEUR).setName("2"));
		
		__inject__(AccountRequestBusiness.class).createMany(List.of(new AccountRequest().setIdentifier("a").setFirstName("a").setLastNames("a").setElectronicMailAddress("a@m.com")
				,new AccountRequest().setIdentifier("b").setFirstName("b").setLastNames("b").setElectronicMailAddress("b@m.com")
				,new AccountRequest().setIdentifier("c").setFirstName("c").setLastNames("c").setElectronicMailAddress("c@m.com")));
			
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(3l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(3l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(0l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(0l);
		
		__inject__(AccountRequestBusiness.class).accept(__inject__(AccountRequestPersistence.class).readBySystemIdentifier("a"));
		
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(3l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(2l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(0l);
		__inject__(AccountRequestBusiness.class).reject(__inject__(AccountRequestPersistence.class).readBySystemIdentifier("b"));
		
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(2l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(1l);
		
		__inject__(AccountRequestBusiness.class).delete(__inject__(AccountRequestPersistence.class).readBySystemIdentifier("c"));
		
		assertThat(__inject__(IdentityPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(AccountRequestPersistence.class).count()).isEqualTo(0l);
		assertThat(__inject__(ActorPersistence.class).count()).isEqualTo(1l);
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(1l);
	}
	
	@Test
	public void rejectedAccountRequest_create() throws Exception{		
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(0l);
		__inject__(RejectedAccountRequestBusiness.class).createMany(List.of(new RejectedAccountRequest().setFirstName("a").setLastNames("a")
				.setElectronicMailAddress("a@m.com").setDate(LocalDateTime.now()).setRequestDate(LocalDateTime.now())));			
		assertThat(__inject__(RejectedAccountRequestPersistence.class).count()).isEqualTo(1l);
	}
	
	@Test
	public void profilePrivilege_createFromPrivileges() throws Exception{
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(0l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1"))).isNull();
		__inject__(ProfilePrivilegeBusiness.class).createFromPrivileges(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")
				, List.of(__inject__(PrivilegePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+1);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1");
		
		profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(1l);
		__inject__(ProfilePrivilegeBusiness.class).createFromPrivileges(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")
				, List.of(__inject__(PrivilegePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1");
	}
	
	@Test
	public void profilePrivilege_createFromProfiles() throws Exception{
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("2"));
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(1l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2"))).isNull();
		__inject__(ProfilePrivilegeBusiness.class).createFromProfiles(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+1);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		
		profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(2l);
		__inject__(ProfilePrivilegeBusiness.class).createFromProfiles(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(ProfilePersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("2");
	}
	
	@Test
	public void profilePrivilege_createFromFunctions() throws Exception{
		EntityCreator.getInstance().createMany(new FunctionType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Function().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Function().setCode("2").setName("2").setTypeFromIdentifier("1"),new Function().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1"),new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1")
				,new Privilege().setCode("4").setName("4").setTypeFromIdentifier("1"),new Privilege().setCode("5").setName("5").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode("1").setName("1"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier("1"),new Profile().setCode("3").setName("3").setTypeFromIdentifier("1"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("2")
				,new ProfilePrivilege().setProfileFromIdentifier("3").setPrivilegeFromIdentifier("3")
				,new ProfilePrivilege().setProfileFromIdentifier("3").setPrivilegeFromIdentifier("4")
				,new ProfilePrivilege().setProfileFromIdentifier("3").setPrivilegeFromIdentifier("5"));
		
		EntityCreator.getInstance().createMany(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2")
				,new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("3"));
		
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		assertThat(profilePrivilegeCount).isEqualTo(5l);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2"))).isNull();
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("3")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("3","4","5");
		
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+2);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		
		profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("1")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("2")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		
		__inject__(ProfilePrivilegeBusiness.class).createFromFunctions(__inject__(ProfilePersistence.class).readBySystemIdentifier("2")
				, List.of(__inject__(FunctionPersistence.class).readBySystemIdentifier("3")));
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+3);
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("1")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2");
		assertThat(PrivilegeQuerier.getInstance().readByProfilesCodes(List.of("2")).stream().map(x->x.getCode()).collect(Collectors.toList())).containsExactly("1","2","3","4","5");
	}
}