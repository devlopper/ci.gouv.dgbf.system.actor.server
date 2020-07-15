package ci.gouv.dgbf.system.actor.server.business.api;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public interface ActorBusiness extends BusinessEntity<Actor> {

	Integer importFromKeycloak();	
	Integer exportToKeycloak();
	
}
