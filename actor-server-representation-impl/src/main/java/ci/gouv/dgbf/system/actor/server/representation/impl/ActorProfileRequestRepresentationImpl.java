package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.business.TransactionResult;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorProfileRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorProfileRequestDto;

@ApplicationScoped
public class ActorProfileRequestRepresentationImpl extends AbstractActorRequestRepresentationImpl<ActorProfileRequestDto,ActorProfileRequest> implements ActorProfileRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected TransactionResult __record__(Collection<String> actorsIdentifiers,Collection<String> requestablesIdentifiers, String actorCode, Boolean ignoreExisting) {
		return __inject__(ActorProfileRequestBusiness.class).record(actorsIdentifiers, requestablesIdentifiers, actorCode, ignoreExisting);
	}

	@Override
	protected TransactionResult __cancel__(Collection<String> identifiers, String actorCode, Boolean ignoreExisting) {
		return __inject__(ActorProfileRequestBusiness.class).cancel(identifiers, actorCode, ignoreExisting);
	}

	@Override
	protected TransactionResult __process__(Collection<ActorProfileRequest> requests, String actorCode) {
		return __inject__(ActorProfileRequestBusiness.class).process(requests, actorCode);
	}

	@Override
	protected Class<ActorProfileRequest> getRequestPersistenceClass() {
		return ActorProfileRequest.class;
	}
}