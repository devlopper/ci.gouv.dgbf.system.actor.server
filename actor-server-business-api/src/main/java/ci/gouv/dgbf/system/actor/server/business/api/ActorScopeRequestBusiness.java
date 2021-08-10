package ci.gouv.dgbf.system.actor.server.business.api;

import java.util.Collection;
import java.util.Map;

import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.business.SpecificBusiness;
import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

public interface ActorScopeRequestBusiness extends SpecificBusiness<ActorScopeRequest> {

	String RECORD = "record";
	@Transactional
	TransactionResult record(Collection<String> actorsIdentifiers,Collection<String> scopesIdentifiers,String actorCode,Boolean ignoreExisting);
	
	String CANCEL = "cancel";
	@Transactional
	TransactionResult cancel(Collection<String> identifiers,String actorCode,Boolean ignoreExisting);
	
	String PROCESS = "process";
	@Transactional
	TransactionResult process(Collection<String> identifiers,Map<String,Boolean> grants,Map<String,String> comments,String actorCode);
	@Transactional
	TransactionResult process(Collection<ActorScopeRequest> actorScopeRequests,String actorCode);
	
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