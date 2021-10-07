package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public abstract class AbstractRequestReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Request> {

	@Override
	protected Class<Request> getEntityClass() {
		return Request.class;
	}
}