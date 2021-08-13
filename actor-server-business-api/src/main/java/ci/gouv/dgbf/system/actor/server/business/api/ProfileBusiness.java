package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.business.SpecificBusiness;
import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public interface ProfileBusiness extends SpecificBusiness<Profile> {
	
	String CREATE_PRIVILEGES = "Profile.createPrivileges";
	@Transactional
	void createPrivileges(Collection<Profile> profiles,Collection<Privilege> privileges);
	
	String DELETE_PRIVILEGES = "Profile.deletePrivileges";
	@Transactional
	void deletePrivileges(Collection<Profile> profiles,Collection<Privilege> privileges);
	
	String SAVE_PRIVILEGES = "Profile.savePrivileges";
	@Transactional
	void savePrivileges(Collection<Profile> profiles,Collection<Privilege> creatablePrivileges,Collection<Privilege> deletablePrivileges);
	
	@Transactional
	void importFormKeycloakRoles();
	
	@Transactional
	void exportToKeycloakRoles();
	
	Collection<String> getCodesByActorCode(String actorCode);
	
	String CREATE = "Profile.create";
	@Transactional
	TransactionResult create(String code,String name,String typeIdentifier,Byte orderNumber,Boolean requestable,String actorCode);
	
	String UPDATE = "Profile.update";
	@Transactional
	TransactionResult update(String identifier,String code,String name,String typeIdentifier,Byte orderNumber,Boolean requestable,String actorCode);
}