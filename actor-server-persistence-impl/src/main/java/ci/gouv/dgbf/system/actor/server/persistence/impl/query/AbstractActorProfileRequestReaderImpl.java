package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;

public abstract class AbstractActorProfileRequestReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<ActorProfileRequest> {

	@Override
	protected Class<ActorProfileRequest> getEntityClass() {
		return ActorProfileRequest.class;
	}
}