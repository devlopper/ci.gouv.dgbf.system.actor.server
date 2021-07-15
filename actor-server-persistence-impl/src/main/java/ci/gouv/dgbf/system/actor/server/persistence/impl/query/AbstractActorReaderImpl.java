package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public abstract class AbstractActorReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Actor> {

	@Override
	protected Class<Actor> getEntityClass() {
		return Actor.class;
	}
}