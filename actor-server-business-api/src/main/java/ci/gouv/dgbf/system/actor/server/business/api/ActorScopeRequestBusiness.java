package ci.gouv.dgbf.system.actor.server.business.api;

import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

public interface ActorScopeRequestBusiness extends AbstractActorRequestBusiness<ActorScopeRequest> {

	String RECORD = "ActorScopeRequest.record";
	
	String CANCEL = "ActorScopeRequest.cancel";
	
	String PROCESS = "ActorScopeRequest.process";
	
	/* rules */
	
	static Boolean isGrantValid(Boolean granted) {
		return granted != null;
	}
	
	static Boolean isProcessingCommentValid(String comment,Boolean granted) {
		if(granted == null || granted)
			return Boolean.TRUE;
		return StringHelper.isNotBlank(comment);
	}
	
	static void validateGrant(Boolean granted) {
		if(isGrantValid(granted))
			return;
		throw new RuntimeException("Spécifier explicitement si <<Oui ou Non>> accorder");
	}
	
	static void validateProcessingComment(Boolean granted,String comment) {
		if(isProcessingCommentValid(comment, granted))
			return;
		if(Boolean.TRUE.equals(granted))
			return;
		throw new RuntimeException("Spécifier explicitement un commentaire");
	}
	
	static void validate(Boolean granted,String comment) {
		validateGrant(granted);
		validateProcessingComment(granted, comment);
	}
}