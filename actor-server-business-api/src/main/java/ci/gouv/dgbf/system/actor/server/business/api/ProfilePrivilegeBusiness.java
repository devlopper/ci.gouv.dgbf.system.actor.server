package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

public interface ProfilePrivilegeBusiness extends BusinessEntity<ProfilePrivilege> {

	@Transactional
	void createFromPrivileges(Profile profile,Collection<Privilege> privileges);
	
	@Transactional
	void createFromPrivileges(Collection<Profile> profiles,Collection<Privilege> privileges);
	
	@Transactional
	void createFromProfiles(Profile profile,Collection<Profile> profiles);
	
	@Transactional
	void createFromProfiles(Collection<Profile> profiles,Collection<Profile> profilesOfPrivileges);
	
	@Transactional
	void createFromFunctions(Collection<Profile> profiles,Collection<Function> functions);
	
	@Transactional
	void createFromFunctions(Profile profile,Collection<Function> functions);
	
	/**/
	
	String SAVE = "ProfilePrivilege.save";
	String CREATE_FROM_PRIVILEGES = "ProfilePrivilege.createFromPrivileges";
	String CREATE_FROM_PROFILES = "ProfilePrivilege.createFromProfiles";
	String CREATE_FROM_FUNCTIONS = "ProfilePrivilege.createFromFunctions";
}