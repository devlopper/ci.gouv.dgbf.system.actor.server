package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.apache.commons.lang3.RegExUtils;
import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public interface ActorBusiness extends BusinessEntity<Actor> {

	@Transactional
	void createPrivilegesFromFunctions(Collection<Actor> actors,Collection<Function> functions);
	
	@Transactional
	void createProfiles(Collection<Actor> actors,Collection<Profile> profiles);
	
	@Transactional
	void deleteProfiles(Collection<Actor> actors,Collection<Profile> profiles);
	
	Integer importFromKeycloak();	
	Integer exportToKeycloak();
	Integer updateToKeycloak();
	void sendUpdatePasswordEmail(Actor actor);
	
	@Transactional
	void savePreferences(Actor actor);
	
	@Transactional
	void saveProfile(Actor actor);
	
	Actor instantiateOneToBeCreatedByPublic();
	
	String CREATE_FROM_PUBLIC = "Actor.createByPublic";
	@Transactional
	void createByPublic(Actor actor);
	
	String CREATE_FROM_PUBLIC_REQUERANT_OF_CELIOPE = "Actor.createRequerantOfCeliopeByPublic";
	@Transactional
	void createRequerantOfCeliopeByPublic(Actor actor);
	
	String CREATE_PRIVILEGES_FROM_FUNCTIONS = "Actor.createPrivilegesFromFunctions";
	String CREATE_PROFILES = "Actor.createProfiles";
	String DELETE_PROFILES = "Actor.deleteProfiles";
	String CREATE_SCOPES = "Actor.createScopes";
	String DELETE_SCOPES = "Actor.deleteScopes";
	String SAVE_PREFERENCES = "Actor.savePreferences";
	String SAVE_PROFILE = "Actor.saveProfile";
	
	/**/
	
	static String generatePassword() {
		return RandomHelper.getAlphabetic(8);
	}
	
	static String normalizeElectronicMailAddress(String electronicMailAddress) {
		electronicMailAddress = StringUtils.stripToNull(electronicMailAddress);
		if(StringHelper.isBlank(electronicMailAddress))
			return null;
		electronicMailAddress = electronicMailAddress.toLowerCase();
		electronicMailAddress = RegExUtils.removeAll(electronicMailAddress, " ");
		return electronicMailAddress;
	}
}