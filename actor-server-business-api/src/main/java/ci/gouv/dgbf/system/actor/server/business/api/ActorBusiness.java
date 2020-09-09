package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;

import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.random.RandomHelper;
import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

public interface ActorBusiness extends BusinessEntity<Actor> {

	@Transactional
	void createPrivilegesFromFunctions(Collection<Actor> actors,Collection<Function> functions);
	
	Integer importFromKeycloak();	
	Integer exportToKeycloak();
	void sendUpdatePasswordEmail(Actor actor);
	
	@Transactional
	void savePreferences(Actor actor);
	
	@Transactional
	void saveProfile(Actor actor);
	
	String CREATE_PRIVILEGES_FROM_FUNCTIONS = "Actor.createPrivilegesFromFunctions";
	String SAVE_PREFERENCES = "Actor.savePreferences";
	String SAVE_PROFILE = "Actor.saveProfile";
	
	/**/
	
	static String generatePassword() {
		return RandomHelper.getAlphanumeric(6);
	}
}