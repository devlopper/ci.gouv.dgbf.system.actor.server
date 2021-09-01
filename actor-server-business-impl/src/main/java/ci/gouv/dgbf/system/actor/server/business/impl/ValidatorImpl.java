package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowablesMessages;
import org.cyk.utility.business.Validator;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileRequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AbstractActorRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class ValidatorImpl extends Validator.AbstractImpl implements Serializable {

	@Override
	protected <T> void __validate__(Class<T> klass, T entity, Object actionIdentifier,ThrowablesMessages throwablesMessages) {
		if(entity instanceof ActorProfileRequest)
			validateActorProfileRequest((ActorProfileRequest) entity, actionIdentifier, throwablesMessages);
		else if(entity instanceof ActorScopeRequest)
			validateActorScopeRequest((ActorScopeRequest) entity, actionIdentifier, throwablesMessages);
		else
			super.__validate__(klass, entity, actionIdentifier, throwablesMessages);
	}
	
	/**/
	
	protected <T> void validateActorProfileRequest(ActorProfileRequest actorProfileRequest, Object actionIdentifier,ThrowablesMessages throwablesMessages) {
		validateActorRequest(actorProfileRequest, actionIdentifier, throwablesMessages,ActorProfileRequestBusiness.PROCESS);
	}
	
	protected <T> void validateActorScopeRequest(ActorScopeRequest actorScopeRequest, Object actionIdentifier,ThrowablesMessages throwablesMessages) {
		validateActorRequest(actorScopeRequest, actionIdentifier, throwablesMessages,ActorScopeRequestBusiness.PROCESS);
	}
	
	protected <T> void validateActorRequest(AbstractActorRequest actorRequest, Object actionIdentifier,ThrowablesMessages throwablesMessages,Object processActionIdentifier) {
		if(processActionIdentifier.equals(actionIdentifier)) {
			if(actorRequest.getGranted() == null)
				throwablesMessages.add(String.format("Spécifier explicitement la valeur accordée de %s", actorRequest.getIdentifier()));
			if(Boolean.FALSE.equals(actorRequest.getGranted()) && StringHelper.isBlank(actorRequest.getProcessingComment()))
				throwablesMessages.add(String.format("Spécifier le motif de rejet de %s", actorRequest.getIdentifier()));
		}
	}

	/**/
	
	public static void validateActorCode(String code,ThrowablesMessages throwablesMessages) {
		throwablesMessages.addIfTrue("nom d'utilisateur requis", StringHelper.isBlank(code));
		throwablesMessages.addIfTrue("utilisateur inconnu", !Boolean.TRUE.equals(CodeExecutor.getInstance().exists(Actor.class, code)));
	}
}