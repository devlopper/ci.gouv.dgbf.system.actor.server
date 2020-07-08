package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import org.cyk.utility.server.business.BusinessEntity;

public interface ActorBusiness extends BusinessEntity<Actor> {

	Integer importFromKeycloak();	
	Integer exportToKeycloak();
	
}
