package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;

public abstract class AbstractActorScopeRequestReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<ActorScopeRequest> {

	@Override
	protected Class<ActorScopeRequest> getEntityClass() {
		return ActorScopeRequest.class;
	}
}