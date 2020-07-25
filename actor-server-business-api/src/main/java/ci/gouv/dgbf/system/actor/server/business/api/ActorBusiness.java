package ci.gouv.dgbf.system.actor.server.business.api;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public interface ActorBusiness extends BusinessEntity<Actor> {

	Integer importFromKeycloak();	
	Integer exportToKeycloak();
	void sendUpdatePasswordEmail(Actor actor);
	
	@Transactional
	void savePreferences(Actor actor);
	
	@Transactional
	void saveProfile(Actor actor);
	
	String SAVE_PREFERENCES = "Actor.savePreferences";
	String SAVE_PROFILE = "Actor.saveProfile";
}
