package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public interface ProfileBusiness extends BusinessEntity<Profile> {
	
	@Transactional
	void createPrivileges(Collection<Profile> profiles,Collection<Privilege> privileges);
	
	@Transactional
	void deletePrivileges(Collection<Profile> profiles,Collection<Privilege> privileges);
	
	@Transactional
	void savePrivileges(Collection<Profile> profiles,Collection<Privilege> creatablePrivileges,Collection<Privilege> deletablePrivileges);
	
	String CREATE_PRIVILEGES = "Profile.createPrivileges";
	String DELETE_PRIVILEGES = "Profile.deletePrivileges";
	String SAVE_PRIVILEGES = "Profile.savePrivileges";
}