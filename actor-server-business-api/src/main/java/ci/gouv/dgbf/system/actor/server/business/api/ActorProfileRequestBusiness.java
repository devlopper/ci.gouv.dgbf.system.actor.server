package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;

public interface ActorProfileRequestBusiness extends AbstractActorRequestBusiness<ActorProfileRequest> {

	String RECORD = "ActorProfileRequest.record";
	
	String CANCEL = "ActorProfileRequest.cancel";
	
	String PROCESS = "ActorProfileRequest.process";	
}
