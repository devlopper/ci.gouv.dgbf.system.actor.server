package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import org.cyk.utility.server.business.BusinessEntity;

public interface RequestBusiness extends BusinessEntity<Request> {

	
	String SAVE = "Request.save";
	
}
