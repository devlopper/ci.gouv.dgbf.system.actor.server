package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.business.EntityCreator;
import org.cyk.utility.server.business.test.arquillian.AbstractBusinessArquillianIntegrationTestWithDefaultDeployment;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

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
		
		EntityCreator.getInstance().createMany(new ProfileType().setCode(ProfileType.CODE_SYSTEM).setName("1"),new ProfileType().setCode(ProfileType.CODE_UTILISATEUR).setName("2"));
		EntityCreator.getInstance().createMany(new Profile().setCode("1").setName("1").setTypeFromIdentifier(ProfileType.CODE_SYSTEM)
				,new Profile().setCode("2").setName("2").setTypeFromIdentifier(ProfileType.CODE_SYSTEM)
				,new Profile().setCode("3").setName("3").setTypeFromIdentifier(ProfileType.CODE_SYSTEM));
		
		EntityCreator.getInstance().createMany(new ProfileFunction().setProfileFromIdentifier("1").setFunctionFromIdentifier("1")
				,new ProfileFunction().setProfileFromIdentifier("2").setFunctionFromIdentifier("2"),new ProfileFunction().setProfileFromIdentifier("3").setFunctionFromIdentifier("2"));
		
		EntityCreator.getInstance().createMany(new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("1")
				,new ProfilePrivilege().setProfileFromIdentifier("1").setPrivilegeFromIdentifier("3"),new ProfilePrivilege().setProfileFromIdentifier("2").setPrivilegeFromIdentifier("2"));
		
		Long profileCount = __inject__(ProfilePersistence.class).count();
		Long actorProfileCount = __inject__(ActorProfilePersistence.class).count();
		Long profilePrivilegeCount = __inject__(ProfilePrivilegePersistence.class).count();
		__inject__(ActorBusiness.class).create(new Actor().setCode("1").setFirstName("yao").setLastNames("yves").setElectronicMailAddress("a@m.com")
				.addFunctionsByIdentifiers("1"));
		assertThat(__inject__(ProfilePersistence.class).count()).isEqualTo(profileCount+1);
		assertThat(__inject__(ActorProfilePersistence.class).count()).isEqualTo(actorProfileCount+1);
		assertThat(__inject__(ProfilePrivilegePersistence.class).count()).isEqualTo(profilePrivilegeCount+2);
	}
}